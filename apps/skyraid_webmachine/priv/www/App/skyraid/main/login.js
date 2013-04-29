define(['skyraid/backend', 'skyraid/home/user', 'durandal/app', 'durandal/plugins/router'], function (backend, user, app, router) {
	
   	return {
        username: ko.observable(),
        password: ko.observable(),

        login: function () {
        	backend.login(this.username(), this.password()).then(function(result) {
                user.displayName(result.user.displayName);
                user.email(result.user.email);
                user.accounts.push.apply(user.accounts, result.user.accounts);
            	router.navigateTo('#home');
        	})
        }
    } 
});