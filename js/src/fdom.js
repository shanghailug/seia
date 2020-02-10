function Location() {
}

function Window() {
}

function DocumentFragment()
{
}

function Node(doc) {
    this.ownerDocument = doc;
    this.appendChild = function() {
    };

    this.innerHTML = "";
}

function Document() {
    this.defaultView = new Window();
    this.head = new Node(this);
    this.body = new Node(this);
    this.getElementById = function(id) {
        var elem = new Node(this);
        return elem;
    };

    this.createDocumentFragment = function() {
        var f = new DocumentFragment();
        return f;
    }

    this.location = new Location();
}

module.exports = {
    Document
};
