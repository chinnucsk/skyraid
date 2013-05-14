define(['./login', './register', 'durandal/app', 'durandal/viewModel', 'durandal/system'], function (Login, Register, app, viewModel, system) {

	var login = Login;
	var register = Register;

    return {
        loginTabActive: ko.observable(),
        registerTabActive: ko.observable(),

        activeView: ko.observable(),

        activate: function(vars) {
            if (vars.op == 'register') {
                this.registerTabActive(true);
                this.loginTabActive(false);
                this.activeView(register);
            }
            else {
                this.loginTabActive(true);
                this.registerTabActive(false);
                this.activeView(login);
            }
        }
    }; 
});