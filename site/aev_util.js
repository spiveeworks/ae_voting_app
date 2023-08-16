export {login, post_json, logged_in, address, request_and_sign, form_and_post};

import * as sk from "/sidekick.js";

// account selection 'login' stuff
var logged_in = false;
var address;


function connect(logger) {
    return sk.connect(
        'ske-connect-1',
        {name: 'sidekick examples',
         version: 1},
        sk.TIMEOUT_DEF_CONNECT_MS,
        "failed to connect to wallet",
        logger
    );
}

// This is the first half of login() from show_poll.html - factor these?
async function login() {
    let logger = sk.cl();

    await connect(logger);
    let wallet_info = await sk.address(
        'ske-address-1',
        {type: 'subscribe',
         value: 'connected'},
        sk.TIMEOUT_DEF_ADDRESS_MS,
        "failed to address to wallet",
        logger
    );
    if (!wallet_info.ok) return false;

    let maybe_address = Object.keys(wallet_info.result.address.current)[0];
    if (maybe_address === undefined) return false;

    address = maybe_address;
    logged_in = true;
    document.getElementById("login_status").innerHTML = "Viewing as " + address;
    document.getElementById("login_button").innerText = "Change account";

    return true;
}

// this also appears in show_poll.html - util module?
async function post_json(url, body) {
    let response = await fetch(
        url,
        {
            method: "post",
            headers: {
                'Accept': 'application/json',
                'Content-Type': 'application/json'
            },
            body: JSON.stringify(body)
        }
    );

    if (response.ok) {
        let body = await response.json();
        return {ok: true, body: body};
    } else {
        return response;
    }
}

function sign_message(logger, message) {
    let promise = sk.msg_sign(
        'ske-msg_sign-1',
        address,
        message,
        sk.TIMEOUT_DEF_MSG_SIGN_MS,
        'sign message timed out',
        logger
    );

    return promise;
}

async function request_and_sign(endpoint, request) {
    if (!logged_in) return {ok: false};

    let logger = sk.cl();

    // message formation

    request.address = address;

    let action_resp = await post_json(endpoint + "/formMessage", request);
    if (!action_resp.ok) return {ok: false};

    let ts = action_resp.body.timestamp;
    let nonce = action_resp.body.nonce;
    let message = action_resp.body.message;

    // sign message

    let sign_result = await sign_message(logger, action_resp.body.message);
    if (!sign_result.ok) return {ok: false};

    // submit signature

    request.timestamp = action_resp.body.timestamp;
    request.nonce = action_resp.body.nonce;
    request.message_signature = sign_result.result.signature;

    let submit_resp = await post_json(endpoint + "/submitSig", request);

    return submit_resp;
}

function sign_tx(logger, tx) {
    let sign_params = {
        tx: tx,
        returnSigned: true,
        networkId: "ae_uat"
    };
    return sk.tx_sign_noprop('sk-tx-sign-1', sign_params, sk.TIMEOUT_DEF_TX_SIGN_NOPROP_MS, 'sign transaction timed out', logger);
}

async function form_and_post(endpoint, request) {
    if (!logged_in) return {ok: false};

    let logger = sk.cl();

    // form tx

    request.address = address;
    let tx_resp = await post_json(endpoint + "/formTransaction", request);
    if (!tx_resp.ok) return {ok: false};

    let tx = tx_resp.body.tx;
    console.log({transaction: tx});

    // sign tx
    let signed_result = await sign_tx(logger, tx);
    if (!signed_result.ok) return {ok: false};

    let signed_tx = signed_result.result.signedTransaction;

    // post tx
    request.signed_tx = signed_result.result.signedTransaction;

    let post_resp = await post_json(endpoint + "/postTransaction", request);
    return post_resp;
}

