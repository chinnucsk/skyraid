define(['skyraid/backend', 'skyraid/home/user', 'durandal/system'], function (backend, user, system) {

    var Name = ko.observable();
    var Provider = ko.observable();
	var Files = ko.observableArray();

   	return {
        name: Name,
        provider: Provider,
		files: Files,

        activate: function() {
			backend.getStorage(this.sessionId, "todo provide name").then(function(result) {
                system.log("hello", this);
				Name(result.storage.name);
		Provider(result.storage.provider);
		Files.push.apply(Files, result.storage.files);
		})
        }
    } 
});