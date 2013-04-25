define(['durandal/app'], function (app) {
	
   	return {
        username: 'username',
        password: 'password',
        email: 'email',

        register: function () {
            app.showMessage('Register !', 'Greetings');
        }
    } 
});