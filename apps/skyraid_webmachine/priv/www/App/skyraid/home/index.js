define(['skyraid/home/user'], function (user) {

   	return {
        displayName: user.displayName,
        accounts: user.accounts,
        activeAccount: user.accounts()[0],  

        storage: {
            name: 'DropBox1',
            provider: 'dropbox',
            files: [
                { name: 'myfile.png', type: "png", size: 122},
                { name: 'myfile.txt', type: "txt", size: 10}
            ]
        } 
    } 
});