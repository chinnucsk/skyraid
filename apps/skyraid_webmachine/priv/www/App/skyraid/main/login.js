define(['skyraid/backend', 'skyraid/home/user', 'durandal/app', 'durandal/plugins/router'], function (backend, user, app, router) {

	var Username = ko.observable();
    var Password = ko.observable();

   	return {
        username: Username,
        password: Password,

        login: function () {
            app.showMessage(this.username());
            
        	backend.login(this.username(), this.password()).then(function(result) {
                user.displayName(result.user.displayName);
                user.email(result.user.email);
                user.accounts.push.apply(user.accounts, result.user.accounts);
            	router.navigateTo('#home');
        	})
        }
    } 
});