define(['durandal/plugins/router'], function (router) {
    
    return {
        router: router,
        activate: function () {
            router.map([
                { url: 'main', moduleId: 'skyraid/main/index', name: 'Main', visible: true },
                { url: 'home', moduleId: 'skyraid/home/index', name: 'Home', visible: true }
            ]);
            
            return router.activate('main');
        }
    };
});