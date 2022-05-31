import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var script = document.createElement('script');
script.setAttribute('type', 'text/javascript');
script.setAttribute('src', 'https://api.mapy.cz/loader.js');
document.head.appendChild(script);

const rootNode = document.getElementById('root')

/* Fetch back generated bytes from the local storage */
function rememberedBytes() {
    const bytes = localStorage.getItem("bytes");
    return bytes ? bytes.split(",").map(x => parseInt(x, 10)) : null;
}

function persistedToken() {
    const token = localStorage.getItem("token")
    return token ? token : null;
}

const app = Elm.Main.init({
    node: rootNode,
    flags: {
        clientId: "1033652281621-os35kv9ie4jnisbmcukrv1fcf14n41u2.apps.googleusercontent.com", //process.env.CLIENT_ID,
        state: rememberedBytes(),
        token: persistedToken()
    }
});

customElements.define('seznam-maps', class extends HTMLElement {
    constructor() {
        super();
        let _screen
    }

    connectedCallback() {
        console.log("Map is loading");
        this._screen = document.createElement('div');
        this._screen.setAttribute("id", "map");
        this._screen.style.height = this.getAttribute('height');
        this._screen.style.width = this.getAttribute('width');
        this.appendChild(this._screen);

        Loader.async = true;
        Loader.load(null, null, this.createMap);
    }

    createMap () {
        this._screen = document.getElementById("map");
        var center = SMap.Coords.fromWGS84(14.41790, 50.12655);
        this._map = new SMap(JAK.gel(this._screen), center, 13);
        this._map.addDefaultLayer(SMap.DEF_BASE).enable();
        this._map.addDefaultControls();
    }

    reload() {
        Loader.load();
        var center = SMap.Coords.fromWGS84(14.41790, 50.12655);
        this._map = new SMap(JAK.gel(this._screen), center, 13);
        this._map.addDefaultLayer(SMap.DEF_BASE).enable();
        this._map.addDefaultControls();
    }
});

/* Generate high entropy random bytes using the Web Crypto API and
remember them so that they are preserved between redirections. This
allows to protect for XSS & authorization code attacks */
app.ports.genRandomBytes.subscribe(n => {
    const buffer = new Uint8Array(n);
    crypto.getRandomValues(buffer);
    const bytes = Array.from(buffer);
    localStorage.setItem("bytes", bytes);
    app.ports.randomBytes.send(bytes);
});

app.ports.persistToken.subscribe(token => {
    console.log("Persisting token")
    localStorage.setItem("token", token)
});

app.ports.removeToken.subscribe(_ => {
    console.log("Removing token")
    localStorage.removeItem("token")
});

// app.ports.reloadMap.subscribe(message => {
//     let elem = document.getElementById('maps')
//     console.log(elem)
//     elem.reload()
// });

// app.ports.errorLog.subscribe((errorText) => {
//     console.error(errorText);
// });

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();