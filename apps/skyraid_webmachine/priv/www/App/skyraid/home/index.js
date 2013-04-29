define(['skyraid/backend', 'skyraid/home/user', 'skyraid/home/storage'], function (backend, user, storage) {

   	return {
        displayName: user.displayName,
        accounts: user.accounts,
        activeAccount: user.accounts()[0]
    } 
});