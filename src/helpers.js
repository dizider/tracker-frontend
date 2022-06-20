window.randDarkColor = function () {
    var lum = -0.25;
    var hex = String('#' + Math.random().toString(16).slice(2, 8).toUpperCase()).replace(/[^0-9a-f]/gi, '');
    if (hex.length < 6) {
        hex = hex[0] + hex[0] + hex[1] + hex[1] + hex[2] + hex[2];
    }
    var rgb = "#",
        c, i;
    for (i = 0; i < 3; i++) {
        c = parseInt(hex.substr(i * 2, 2), 16);
        c = Math.round(Math.min(Math.max(0, c + (c * lum)), 255)).toString(16);
        rgb += ("00" + c).substr(c.length);
    }
    return rgb;
}

window.rememberedBytes = function () {
    const bytes = localStorage.getItem("bytes");
    return bytes ? bytes.split(",").map(x => parseInt(x, 10)) : null;
}

window.persistedToken = function () {
    const token = localStorage.getItem("token")
    return token ? token : null;
}

window.generateRandomBytes = function (n) {
    const buffer = new Uint8Array(n);
    crypto.getRandomValues(buffer);
    const bytes = Array.from(buffer);
    localStorage.setItem("bytes", bytes);
    return bytes;
}