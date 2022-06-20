let lastPosMarkerCard
let liveMarkers = {} // last positions of all live tracks

window.customElements.define('seznam-maps', class extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        console.log("Map is loading");
        maps = this
        this.loadMap()
    }

    loadMap() {
        Loader.lang = "en";
        Loader.async = true;
        Loader.load(null, null, this.createMap);
    }

    createMap() {
        this._screen = document.getElementById("map");
        var center = SMap.Coords.fromWGS84(14.41790, 50.12655);
        map = new SMap(JAK.gel(this._screen), center, 13);

        var sync = new SMap.Control.Sync({ bottomSpace: 0 });
        map.addControl(sync);

        map.addDefaultLayer(SMap.DEF_TURIST).enable()
        map.addDefaultLayer(SMap.DEF_OPHOTO)
        map.addDefaultLayer(SMap.DEF_BASE)
        map.addDefaultControls();

        let layerSwitch = new SMap.Control.Layer({ width: 65, items: 3, page: 3 })
        layerSwitch.addDefaultLayer(SMap.DEF_BASE)
        layerSwitch.addDefaultLayer(SMap.DEF_OPHOTO)
        layerSwitch.addDefaultLayer(SMap.DEF_TURIST)

        map.addControl(layerSwitch, { left: "8px", top: "50px" })

        markersLayer = new SMap.Layer.Marker()
        map.addLayer(markersLayer)
        markersLayer.enable()

        map.getSignals().addListener(window, "map-click", function (e) {
            console.log("map clicked")
            // document.getElementById("maps").requestFullscreen();
        });

        wsSocket.addEventListener('open', function (event) {
            console.log('Server WS connected!');
        });

        wsSocket.addEventListener('message', function (event) {
            if (event.data.startsWith("!!")) {
                console.log('Message from server ' + event.data);
                alert("WS: " + event.data)
                return
            }

            let coords = JSON.parse(event.data)
            console.log("Received new coords: " + JSON.stringify(coords))

            app.ports.newCoordinatesReceived.send(event.data);
        });
    }

    addTrack(track) {
        let responseText = track[1]
        let trackId = track[0]

        wsSocket.send("coordinates/" + trackId)

        let xmlDoc = JAK.XML.createDocument(responseText)
        let pts = xmlDoc.getElementsByTagName("trkpt")
        let lastPoint = pts[pts.length - 1];
        if (lastPoint != undefined) {
            let lineColor = randDarkColor();

            let center = SMap.Coords.fromWGS84(lastPoint.getAttribute("lon"), lastPoint.getAttribute("lat"))
            map.setCenter(center)

            let img = winUrl + "/drop.svg"
            console.log(img)
            let desc = "Track id: " + trackId + "</br> time: " + lastPoint.getElementsByTagName("time")[0].innerHTML + "</br> battery: " + lastPoint.getAttribute("batt")
            let marker = makeMarker(lastPoint.getAttribute("lat"), lastPoint.getAttribute("lon"), trackId, desc, img)
            markersLayer.addMarker(marker)
            // pass the rest to draw the line
            let gpx = new SMap.Layer.GPX(xmlDoc, null, { maxPoints: 5000, colors: [lineColor] })
            map.addLayer(gpx)
            gpx.enable()
        }
    }

    updateCoordinates(coords) {
        markersLayer.removeAll()
        for (let i in coords) {
            let c = coords[i]
            let img = winUrl + "/drop.svg"
            let desc = "Track id: " + c.trackId + "</br> time: " + c.time + "</br> battery: " + c.battery
            let marker = makeMarker(c.lat, c.lon, c.trackId, desc, img)
            liveMarkers = {}
            markersLayer.addMarker(marker)
            liveMarkers[c.trackId] = marker
        }
    }
});

function makeMarker(lat, lon, title, text, img) {
    lastPosMarkerCard = new SMap.Card()
    lastPosMarkerCard.getHeader().innerHTML = "<strong>" + title + "</strong>"
    lastPosMarkerCard.getBody().innerHTML = text

    let markerContent = JAK.mel("div")
    let pic = JAK.mel("img", { src: img }, { "height": "30px" })
    markerContent.appendChild(pic)

    let markerTitle = JAK.mel("div", {}, {
        position: "absolute",
        left: "0px",
        top: "0px",
        textAlign: "center",
        width: "22px",
        color: "white",
        textShadow: "-1px 0 black, 0 1px black, 1px 0 black, 0 -1px black",
        fontWeight: "bold"
    })
    markerTitle.innerHTML = title
    markerContent.appendChild(markerTitle)

    let coords = SMap.Coords.fromWGS84(lon, lat);
    let marker = new SMap.Marker(coords, null, { url: markerContent });
    marker.decorate(SMap.Marker.Feature.Card, lastPosMarkerCard)
    return marker
}