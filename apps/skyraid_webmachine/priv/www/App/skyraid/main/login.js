define(['skyraid/backend', 'skyraid/home/user', 'durandal/app', 'durandal/plugins/router'], function (backend, user, app, router) {

	var Username = ko.observable();
    var Password = ko.observable();

    var Alerts = ko.observableArray([]);

    Login = function () {
        backend.login(Username(), Password()).then(function(result) {
            user.displayName(result.user.displayName);
            user.email(result.user.email);
            user.accounts.push.apply(user.accounts, result.user.accounts);
            router.navigateTo('#home');
        }, function(jqxhr) {
            var response = $.parseJSON(jqxhr.responseText);
            Alerts.push({message: response.error, priority: 'error'});
        });
    };

    return {
        username: Username,
        password: Password,

        alerts: Alerts,

        login: Login
    }; 
});