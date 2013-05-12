define(['skyraid/backend','skyraid/home/user', 'durandal/app', 'durandal/plugins/router'], function (backend, user, app, router) {
	
    var Username = ko.observable();
    var Password = ko.observable();
    var Email = ko.observable();

    var Alerts = ko.observableArray([]);

    var Register = function () {
            backend.register(Username(), Password(), Email()).then(function(result) {
                if (result.status == 'ok') {
                    backend.login(this.username, this.password).then(function(result) {
                        user.displayName(result.user.displayName);
                        user.email(result.user.email);
                        user.accounts.push.apply(user.accounts, result.user.accounts);
                        router.navigateTo('#home');
                    });
                } else {
                    Alerts.push({message: result.error, priority: 'error'});
                }
            }, function(jqxhr) {
                var response = $.parseJSON(jqxhr.responseText);
                Alerts.push({message: response.error, priority: 'error'});
            })
        };

   	return {
        username: Username,
        password: Password,
        email: Email,

        alerts: Alerts,

        register: Register
    } 
});