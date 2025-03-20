const express = require('express');
const { createProxyMiddleware } = require('http-proxy-middleware');
const cookieParser = require('cookie-parser');

const app = express();
app.use(cookieParser());

const TARGET_URL = 'https://035tscvl-8084.inc1.devtunnels.ms/dist/index_bundle.js';

// Middleware to set a cookie before redirecting
app.use((req, res, next) => {
    res.cookie('.Tunnels.Relay.WebForwarding.Cookies', 'CfDJ8Cs4yarcs6pKkdu0hlKHsZuPyMz--ZB1F0EtCONUqok-Xp1MjobtKrzg_IIrUV1_-X-pXvsPMiaeh86IXXjp2RK4aDK5KWs-ExSTAuacFtXhPJTkBVNDMQBIregZMAQKky4SlRM3B8Y23xbGU7OJFAObWBbM3bUH17LWxTY4XV-3cT7eHT3cNx4WIQyDG1QOZPM7AuCi3lKB7zOs29Op2ZkEEAXipn3F5h6qhZOw4N3CQZH5zr-0nqM9zMjW1jJyuzowkv9dc-m1-TJyKF98kG-0nFbFVqRub_-s8YssYsvK4EwmrRO-1BEU0tbGeUzj2QpGifR2fMNrn9OE90Btr61iQlknWimSdC-BJXGcRLk-eVIkcmBbHvXR-XIXbpjWscXmCMNwcPgAg1fQhkpeMe1Lz0uCMnJgQMesJ6x5Dh7dU78qI76XcDSfZFir7Ao4lVSzc22-SgXfj7hE9U3kXOaMjltj8DSAFAD81VAKIF-qBcxCtJcPJJ6J6trqLqSAO6A5DI2bdRuWbyioldWe8s6DHa_hAqDenEqS5AxPCzsQ6nmejhCcD9fKKFJ-cOQiYYvfyrtM8Ip2kfGkC_vn4ogsJTk4yJ8SKOlLBIFTE-hPC0fUkv3Y7u8hsZq4vgCNPG3TPhWF9yOr9kZd0VYoTrG6l7AZyqIWsZEZ25fXbrgcCl-rz3kFBGPetVNVg3dVllWqZ4JdE-0DUcnJlRgBB-MdBFViUptvsJWJ6tTI1nhXjvt2lnOnkiWZDvmxwmR-42-dNwW0mflTQyPGK1fHacotx8osq65lq2B1Drm9j-loYK4oeZGxhJ9Y0jvc4pDnXQ1hwc42BPD-XpJpWHOs16GmgOpZRBOI5kJlhRRR2GdURXtT4JPTDIpAbv7SZs5maT_6AiR1nSD_1sR-50eHlpH4RfEeTjkJxwcYxpKsxIPQCdZKqmf0renvHvXfFB3rwfwyspyzjZwMnxg8216wAs84AdciDr0WkG0ogisnZuuKjuQu9Z4CoFe15Ib8wkUxRXTrI_ly8UVtUw8xNzKj2N-bzk6ghhNmck0fXxYlh3VSTAepyBTTqm_ThOaXw6ZdIS-ekwXbgUDyyT_Uq1zvLFLDPEZ8d5cE1zLkIRLPjRDc4-pYuHxalPVOq7af2PZmncA2t67qcQKRZQz6i5t3ttFaMCFAOmidqK6LrymYQCSlYniSVuT7_eQ48AO-peEd35hUqfj6ie8KoIPAxEqci1idq6H1ASzJQKCVBGsp5oOLNYC_eso6w9hk9UDYfkl1o6g1lbn58Kh_usCkYAsMz-n1esbtkopjuneWJkCv42-C4vs59W-Y7f9mtKJ32wEHFrr72A248fPVxYxmfmubrHZBUf_H5HV3giWqDqZTEIp5tqYIFoE00xbvrK6pCIPSthtl4mTxM3G_cOfUaO1smrEMVmMIdEDIxbRnl2LmkmdKT1JKAzs7fu3wbf6_0uHabQt4foMcpDrE70CG2vDe8TbiE4SUcW4RdUy0TOtJXInl');
    res.cookie('tunnel_phishing_protection','happy-plane-m1cjf0k.inc1')
    next();
});

// Proxy requests to /dist/index_bundle.js
app.use('/dist/index_bundle.js', createProxyMiddleware({
    target: TARGET_URL,
    changeOrigin: true,
    secure: true, // Set to true if using a valid SSL cert
    pathRewrite: { '^/dist/index_bundle.js': '/dist/index_bundle.js' }, // Ensure the correct path is used
    onProxyReq: (proxyReq, req, res) => {
        const cookieHeader = req.headers.cookie || '';
        proxyReq.setHeader('Cookie', cookieHeader);
    }
}));

app.listen(8084, () => {
    console.log('Proxy server running on http://localhost:8084');
});
