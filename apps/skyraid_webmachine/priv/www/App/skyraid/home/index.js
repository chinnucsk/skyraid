define(function () {

   	return {

        accounts: [{
            name: 'DropBox1'
        },{
            name: 'FTP1'
        }],

        accountDetails: {
            name: 'DropBox1',
            provider: "dropbox",
            quotaTotal: 200,
            quotaUsed: 100  
        },

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