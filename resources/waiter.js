if (!window.__xhrCount) {
    var old = XMLHttpRequest.prototype.open;
    window.__xhrCount = 0;
    XMLHttpRequest.prototype.open = function () {
        window.__xhrCount++;
        this.addEventListener('readystatechange', function () {
            if (this.readyState === 4) {
                window.__xhrCount--;
            }
        }, false);
        old.apply(this, arguments);
    }
}