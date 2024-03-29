import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import './helpers'
import './map'
import './mapLoader'

// prevents zooming issues
var meta = document.createElement('meta');
meta.setAttribute('name', 'viewport')
meta.setAttribute('content', 'width=device-width,user-scalable=0,initial-scale=1,minimum-scale=1,maximum-scale=1,viewport-fit=cover')
document.head.appendChild(meta);

const rootNode = document.getElementById('root')

window.app = null
window.map = null
window.maps = null
window.markersLayer = null
window.rootUrl = process.env.ELM_APP_WS_ORIGIN //window.location.host
window.winUrl = "https://" + window.location.host
window.wsUrl = "wss://" + rootUrl + "/subscribe"
window.wsSocket = null
window.tracks = []

console.log(wsUrl)

initApp()

function initApp() {
    app = Elm.Main.init({
        node: rootNode,
        flags: {
            clientId: process.env.ELM_APP_CLIENT_ID,
            apiOrigin: process.env.ELM_APP_API_ORIGIN,
            state: rememberedBytes(),
            token: persistedToken()
        }
    });

    wsSocket = new WebSocket(wsUrl)

    app.ports.genRandomBytes.subscribe(n => {
        const bytes = generateRandomBytes(n);
        app.ports.randomBytes.send(bytes);
    });

    app.ports.persistToken.subscribe(token => {
        localStorage.setItem("token", token)
    });

    app.ports.removeToken.subscribe(_ => {
        localStorage.removeItem("token")
    });

    app.ports.addTrack.subscribe(track => {
        tracks.push(track)
    });

    app.ports.removeTrack.subscribe(track => {
        tracks = tracks.filter(x => {
            return x[0] != track
        })
    })

    app.ports.updateTracks.subscribe(tracks => {
        if(maps)
            maps.addTrack(tracks)
    });

    app.ports.fullscreenMap.subscribe(_ => {
        if (document.fullscreenElement === null)
            maps.requestFullscreen();
        else
            document.exitFullscreen();
    });

    app.ports.subscribeCoordinates.subscribe(trackId => {
        wsSocket.send("coordinates/" + trackId);
    });

    app.ports.unsubscribeCoordinates.subscribe(trackId => {
    });

    app.ports.loadMap.subscribe(_ => {
        if (maps !== undefined)
            maps.redrawMap()
    });

    app.ports.centerMap.subscribe(coords => {
        maps.centerMap(coords)
    })

    document.addEventListener("fullscreenchange", () => {
        app.ports.fullscreenActive.send(document.fullscreenElement !== null)
    });

    // If you want your app to work offline and load faster, you can change
    // unregister() to register() below. Note this comes with some pitfalls.
    // Learn more about service workers: https://bit.ly/CRA-PWA
    serviceWorker.register();
}