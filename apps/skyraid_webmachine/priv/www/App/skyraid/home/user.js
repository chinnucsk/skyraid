define(function () {
	var DisplayName = ko.observable("Adam");
	var Email = ko.observable();
	var SessionId = ko.observable();
	var Accounts = ko.observableArray([{id: "12", name: "Drop1", provider: "dropbox", quotaTotal:500, quotaUsed: 200}]);

   	return {
   		displayName: DisplayName,
   		email: Email,
   		sessionId: SessionId,
   		accounts: Accounts
    } 
});