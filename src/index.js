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
window.rootUrl = "***REMOVED***" //window.location.host
window.winUrl = "http://" + window.location.host
window.wsUrl = "wss://" + rootUrl + "/subscribe";

window.wsSocket = null

console.log(wsUrl)

initApp()

function initApp() {
    Loader.load()
    app = Elm.Main.init({
        node: rootNode,
        flags: {
            clientId: process.env.CLIENT_ID,
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
        maps.addTrack(track)
    });

    app.ports.updateCoordinates.subscribe(coords => {
        console.log("Updating coordinates", coords)
        maps.updateCoordinates(coords)
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
        wsSocket.send("coordinates/" + trackId);
    });

    document.addEventListener("fullscreenchange", () => {
        app.ports.fullscreenActive.send(document.fullscreenElement !== null)
    });

    // If you want your app to work offline and load faster, you can change
    // unregister() to register() below. Note this comes with some pitfalls.
    // Learn more about service workers: https://bit.ly/CRA-PWA
    serviceWorker.register();
}