define(['durandal/system'], function (system) {
    return {
        authenticate:function(Provider) {
            //do some ajax and return a promise
        },

        register:function(Token) {
            //do some ajax and return a promise
        },

        register:function(Username, Password, Email) {
            var register = {username: Username, password: "test", email: "mymail"};

            var promise = $.ajax({
                type: "PUT",
                contentType:"application/json; charset=utf-8",
                dataType: "json",
                url: "http://localhost:80/api/user",
                data: JSON.stringify(register),
                processData: false
            });

            return promise;
        },          

        login:function(Token) {
            //do some ajax and return a promise
        },

        login:function(Username, Password) {
            var result = {
                status: 'ok', 
                sessionId: 'YourSessionId',
                user: {
                    displayName: 'Adam',
                    email: 'my@gmail.com',
                    accounts: [
                        {id: "12", name: "Drop12", provider: "dropbox"},    
                        {id: "33", name: "Drop33", provider: "dropbox"}    
                    ]
                }
            };

            var dfr = $.Deferred();
            dfr.resolve(result);
            return dfr;
        },

        logout:function() {
            //do some ajax and return a promise
        },

        getStorage: function(SessionId, name) {
            var result = {
                status: 'ok',
                storage: {
                    name: 'DropBox1',
                    provider: 'dropbox',
                    files: [
                        { name: 'myfile.png', type: "png", size: 1220},
                        { name: 'myfile.txt', type: "txt", size: 10}
                    ]
                }
            };

            var dfr = $.Deferred();
            dfr.resolve(result);
            return dfr;
        },

        getLocale: function(Locale) {
            return {
                username_exist: "Username already exist"
            }
        }
    };
});