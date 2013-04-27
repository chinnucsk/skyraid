define(['skyraid/backend', 'durandal/app', 'durandal/plugins/router'], function (backend, app, router) {
	
   	return {
        username: ko.observable(),
        password: ko.observable(),

        login: function () {
        	backend.login(this.username(), this.password()).then(function(result) {
            	router.navigateTo('#home');
        	})
        }
    } 
});