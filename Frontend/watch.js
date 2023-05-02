const Webpack = require('webpack');
const WebpackDevServer = require('webpack-dev-server');

const argv = require('yargs/yargs')(process.argv.slice(2)).argv;
const webpackConfig = require(argv.config)(argv.env, { mode: "development" });

const compiler = Webpack(webpackConfig);

const {port, host} = webpackConfig.devServer;
const startServer = () => {
    if(!startServer.promise) {
        startServer.promise = new Promise((resolve, reject) => {
            let server = new WebpackDevServer(compiler, {
                ...webpackConfig.devServer,
                progress: true
            });
            server.listen(port, host, (err) => {
                if(err) {
                    reject(err);
                } else {
                    server.middleware.waitUntilValid(() => {
                        resolve(server);
                    });
                }
            });
        });
    }
    return startServer.promise;
};

let lastMessage = null;
const SUSPEND = "suspend\n";
const READY = "ready\n";
process.stdin.setEncoding('utf8');
process.stdin.on('readable', async function() {
    let data;
    while ((data = this.read()) !== null) {
        if (data === SUSPEND) {
            if(lastMessage === READY) {
                const server = await startServer();
                server.middleware.context.watching.suspend();
            }
        }
        if (data === READY) {
            const server = await startServer();
            server.middleware.context.watching.resume();
        }
        lastMessage = data;
    }
});


