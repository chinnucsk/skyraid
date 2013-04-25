define(['durandal/app'], function (app) {
	
   	return {
        username: 'Adam',
        password: 'test',

        login: function () {
            app.showMessage('Hello !', 'Greetings');
        }
    } 
});