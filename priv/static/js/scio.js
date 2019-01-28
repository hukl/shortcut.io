function ready(fn) {
    if (document.attachEvent ? document.readyState === "complete" : document.readyState !== "loading"){
        fn();
    } else {
        document.addEventListener("DOMContentLoaded", fn);
    }
}

ready(function() {
    new Scio();
});


var Scio = function() {
    this.initialize = function() {
        this.bind_login_form();
        this.bind_sign_up_form();
    };

    this.bind_login_form = function() {
        let login_button = document.querySelector("#login_form button");
        if (login_button) {
            login_button.addEventListener("click", function(event) {
                event.preventDefault();
                this.login();
            }.bind(this));
        }
    };

    this.bind_sign_up_form = function() {
        let signup_button = document.querySelector("#signup_form button");
        if (signup_button) {
            signup_button.addEventListener("click", function(event) {
                event.preventDefault();
                this.signup();
            }.bind(this));
        }
    };

    this.login = function() {
        let form_element = document.querySelector("#login_form"),
            form_data    = new FormData(form_element),
            payload = {
                "email"    : form_data.get("email"),
                "password" : form_data.get("password")
            };

        this.post_form("/sessions", payload)
            .then(data => function(data) {
                if (data.status === 201) {
                    window.location.assign(data.headers.get("location"));
                } else {
                    console.log("Invalid Login Credentials");
                }
            }(data))
            .catch(error => console.error(error));
    };

    this.signup = function() {
        let form_element = document.querySelector("#signup_form"),
            form_data    = new FormData(form_element),
            payload = {
                "email"    : form_data.get("email"),
                "username" : form_data.get("username"),
                "password" : form_data.get("password")
            };

        this.post_form("/users", payload)
            .then(data => function(data) {
                if (data.status === 201) {
                    window.location.assign(data.headers.get("location"));
                } else {
                    console.log(data.error);
                    return data.json();
                }
            }(data))
            .then(json => console.log(json.error))
            .catch(error => console.error(error));
    };

    this.post_form = function(url = "", data = {}) {
        return fetch(url, {
            method: "POST",
            mode: "cors", // no-cors, cors, *same-origin
            cache: "no-cache",
            credentials: "same-origin",
            headers: {
                "Content-Type": "application/json",
            },
            redirect: "follow", // manual, *follow, error
            referrer: "no-referrer", // no-referrer, *client
            body: JSON.stringify(data), // body data type must match "Content-Type" header
        })
            .then(response => response);
    };

    this.initialize();
};
