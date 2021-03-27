(function() {

    function sendJson(url, method, data) {
        const options = {
            method: method,
            body: JSON.stringify(data),
            headers: {
                'Content-Type': 'application/json'
            },
        };

        return fetch(url, options).then((response) => {
            if (!response.ok) {
                return getErrorMessage(response).then((msg) => Promise.reject(msg));
            }

            return response.json();
        });
    }

    function getErrorMessage(response) {
        if (isJsonResponse(response)) {
            return response.json().then((data) => data.message);
        } else {
            return response.text();
        }
    }

    function isJsonResponse(response) {
        const contentType = response.headers.get('content-type');
        return contentType && contentType.includes('application/json');
    }

    window.XHR = {
        delete: function(url) {
            const options = {
                method: 'DELETE',
            };

            return fetch(url, options).then((response) => {
                if (!response.ok) {
                    return Promise.reject(response);
                }

                return null;
            });
        },

        jsonPut: function(url, data) {
            return sendJson(url, 'PUT', data);
        },

        jsonPost: function(url, data) {
            return sendJson(url, 'POST', data);
        },
    };

})();
