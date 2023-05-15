/**
 * This webpack development server wrapper coordinates between an external (purescript) compiler watch process and
 * the webpack watch process. This is needed due to poor integration between the two tools.
 *
 * This server listens on stdin for ready/suspend messages and will start the webpack server only after recieving
 * a ready message. It will pause webpacks compilation/bundling upon recieving a suspend request
 * and will resume after it recieves a ready.
 *
 * The external compiler watch service is expected to send suspend before it begins work
 * and ready after it has completed its work.
 */
const Webpack = require('webpack');
const WebpackDevServer = require('webpack-dev-server');

const argv = require('yargs/yargs')(process.argv.slice(2)).argv;
const webpackConfig = require(argv.config)(argv.env, { mode: "development" });
if(argv.entry) {
    // Override the configs entry with argv, this assumes the configs entry
    // point is called app
    webpackConfig.entry.app = argv.entry;
}

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
                    // Wait until webpack-dev-middleware tells us it is ready
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
            // If we haven't already recieved a ready then do nothing
            // this prevents the server from starting and immediately suspending
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


