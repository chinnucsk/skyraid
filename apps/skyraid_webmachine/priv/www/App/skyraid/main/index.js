define(['./login', './register', 'durandal/app', 'durandal/viewModel'], function (Login, Register, app, viewModel) {

	var login = Login;
	var register = Register;

   	return {
        activeView: login,

        showLogin: function() {
        	this.activeView = login
        },

        showRegister: function() {
        	this.activeView = register
        }
    } 
});