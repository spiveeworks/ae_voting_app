/**
 * # tl;dr
 *
 * 1. {@link detect} the wallet
 * 2. {@link connect} to the wallet
 * 3. Get the wallet's {@link address}
 *
 * From there you can do one of two things
 *
 * 1. Have the wallet sign transactions ({@link tx_sign_noprop})
 * 2. Have the wallet sign arbitrary messages ({@link msg_sign})
 *
 * Forming the transactions and propagating them into the network is your
 * problem.
 *
 * You need a {@link Logger} for most calls. Probably you want {@link cl}. You
 * can write your own if you want but why would you complicate your life like
 * that.
 *
 * @module
 */
// TODONE: add standardized logging interface
// TODONE: logging hooks
// TODONE: invoice
// TODONE: get connect done
// TODONE: make the message queue for responses a map, fill the message queue properly
// TODONE: get it working with superhero
// TODO: jrx
// like: console, http, etc
// allow someone to pass a logger
//-----------------------------------------------------------------------------
// EXPORTS
//-----------------------------------------------------------------------------
export { 
// debugging
hello, unsafe, ok, error, 
// timeout errors
ERROR_CODE_SkTimeoutError, sk_timeout, WebScale, wsl, ConsoleLogger, cl, HttpLogger, http_log, SeqLogger, logger_foreach, 
// timeouts
// time constants
MS, SEC, MIN, HR, 
// timeouts
TIMEOUT_DEF_DETECT_MS, TIMEOUT_DEF_CONNECT_MS, TIMEOUT_DEF_ADDRESS_MS, TIMEOUT_DEF_TX_SIGN_NOPROP_MS, TIMEOUT_DEF_MSG_SIGN_MS, 
// API
// detect
detect, CAPListener, 
// connect
connect, address, tx_sign_noprop, msg_sign, 
// internals
sleep, bulrtot };
//-----------------------------------------------------------------------------
// API
//-----------------------------------------------------------------------------
/**
 * Just `console.log`s `hello`. Use for debugging/to check if import worked correctly
 */
function hello() {
    console.log('hello');
}
/**
 * Constructs an `Ok` value from a pure value
 */
function ok(x) {
    return { ok: true, result: x };
}
/**
 * Constructs an `Error` value from a pure value
 */
function error(x) {
    return { ok: false, error: x };
}
/**
 * Takes a `Safe` value, if `ok`, returns the `ok_t`, or if an error throws the
 * `err_t`
 */
function unsafe(x) {
    if (x.ok)
        return x.result;
    else
        throw x.error;
}
/** Error code for `SkTimeoutError`s */
const ERROR_CODE_SkTimeoutError = 420;
/**
 * Construct a `SkTimeoutError`
 */
function sk_timeout(message, data) {
    return { code: 420,
        message: message,
        data: data };
}
/**
 * It's web scale
 */
function wsl() { return new WebScale(); }
/** construct a `ConsoleLogger` */
function cl() { return new ConsoleLogger(); }
/**
 * Web scale
 */
class WebScale {
    async debug(_msg, _data) { return; }
    async info(_msg, _data) { return; }
    async warning(_msg, _data) { return; }
    async error(_msg, _data) { return; }
}
/**
 * does console.log
 */
class ConsoleLogger {
    listener;
    constructor() {
        let this_ptr = this;
        this.listener =
            function (e) {
                // for typescript
                if (e instanceof MessageEvent) {
                    this_ptr.debug(`ConsoleLogger received MessageEvent`, e.data);
                }
            };
    }
    async debug(_msg, _data) { console.debug(_msg, _data); }
    async info(_msg, _data) { console.log(_msg, _data); }
    async warning(_msg, _data) { console.warn(_msg, _data); }
    async error(_msg, _data) { console.error(_msg, _data); }
    listen() { window.addEventListener('message', this.listener); }
    ignore() { window.removeEventListener('message', this.listener); }
}
/**
 * posts request to an HTTP server
 */
class HttpLogger {
    post_endpoint;
    listener;
    constructor(post_endpoint) {
        this.post_endpoint = post_endpoint;
        let this_ptr = this;
        this.listener =
            function (e) {
                // for typescript
                if (e instanceof MessageEvent) {
                    this_ptr.debug(`HttpLogger received MessageEvent`, e.data);
                }
            };
    }
    async debug(_msg, _data) { http_log(this.post_endpoint, 'debug', _msg, _data); }
    async info(_msg, _data) { http_log(this.post_endpoint, 'info', _msg, _data); }
    async warning(_msg, _data) { http_log(this.post_endpoint, 'warning', _msg, _data); }
    async error(_msg, _data) { http_log(this.post_endpoint, 'error', _msg, _data); }
    listen() { window.addEventListener('message', this.listener); }
    ignore() { window.removeEventListener('message', this.listener); }
}
async function http_log(post_endpoint, log_level, message, data) {
    try {
        await fetch(post_endpoint, { method: 'POST',
            body: JSON.stringify({ level: log_level,
                message: message,
                data: data }, undefined, 4) });
    }
    catch (e) {
        console.error(e);
    }
}
/**
 * have an array of loggers fire sequentially
 */
class SeqLogger {
    loggers;
    constructor(loggers) { this.loggers = loggers; }
    async debug(_msg, _data) { logger_foreach(this.loggers, 'debug', _msg, _data); }
    async info(_msg, _data) { logger_foreach(this.loggers, 'info', _msg, _data); }
    async warning(_msg, _data) { logger_foreach(this.loggers, 'warning', _msg, _data); }
    async error(_msg, _data) { logger_foreach(this.loggers, 'error', _msg, _data); }
}
async function logger_foreach(loggers, log_level, message, data) {
    for (let logger of loggers) {
        switch (log_level) {
            case 'debug':
                logger.debug(message, data);
                break;
            case 'info':
                logger.info(message, data);
                break;
            case 'warning':
                logger.warning(message, data);
                break;
            case 'error':
                logger.error(message, data);
                break;
        }
    }
}
//-----------------------------------------------------------------------------
// ACTUAL API
//-----------------------------------------------------------------------------
// UNITS OF TIME
/** Unit of time; `const MS = 1` */
const MS = 1;
/** `const SEC = 1000*MS` */
const SEC = 1000 * MS;
/** `const MIN = 1000*MS` */
const MIN = 60 * SEC;
/** `const HR = 60*MIN` */
const HR = 60 * MIN;
// TIMEOUTS
/**
 * 7 seconds. Superhero announces itself every 3 seconds, so this is 2
 * announcements + 1 second.
 */
const TIMEOUT_DEF_DETECT_MS = 7 * SEC;
/**
 * 1 second (instaneous in practice)
 */
const TIMEOUT_DEF_CONNECT_MS = 1 * SEC;
/**
 * In the general case, this pops up the modal that requires the user to
 * manually confirm, so is set to 5 minutes. This is where you as the developer
 * need to exercise some discretion.
 */
const TIMEOUT_DEF_ADDRESS_MS = 5 * MIN;
/**
 * In the general case, this pops up the modal that requires the user to
 * manually confirm, so is set to 5 minutes. This is an instance where you as
 * the developer need to exercise some discretion.
 */
const TIMEOUT_DEF_TX_SIGN_NOPROP_MS = 5 * MIN;
/**
 * In the general case, this pops up the modal that requires the user to
 * manually confirm, so is set to 5 minutes. This is an instance where you as
 * the developer need to exercise some discretion.
 */
const TIMEOUT_DEF_MSG_SIGN_MS = 5 * MIN;
// Ah ok
//
// so for connection.announcePresence, we just listen and recieve, unwrap, send back
//
// for everything else, we send a request, await a response
//
// either: SkTimeoutError or an RpcError
//-----------------------------------------------------------------------------
// API: detection
//-----------------------------------------------------------------------------
/**
 * This is the first step in connecting to the wallet. Before doing anything
 * else, you have to wait for the wallet to announce itself.
 *
 * This function waits for the wallet to announce itself
 *
 * Wait for wallet to announce itself, and then return.
 *
 * @example
 * ```ts
 * // document has a button with id=detect
 * // this is the function that is triggered when the button is clicked
 * async function detect(logger: sk.Logger): Promise<void> {
 *     let h4 = document.getElementById("detected")!;
 *     let pre = document.getElementById("detect-info")!;
 *
 *     h4.innerHTML = 'detecting...';
 *     h4.style.color = 'GoldenRod';
 *
 *     // try to detect the wallet
 *     // will fail on timeout error
 *     // console logger
 *     let maybe_wallet_info =//: sk.Safe<awcp.EventData_W2A_connection_announcePresence, sk.TimeoutError> =
 *             await sk.detect(sk.TIMEOUT_DEF_DETECT_MS, "failed to detect wallet", logger);
 *
 *     console.log(maybe_wallet_info);
 *
 *     // ok means wallet was detected
 *     if (maybe_wallet_info.ok) {
 *         h4.innerHTML   = "detected";
 *         h4.style.color = "green";
 *         pre.innerHTML  = JSON.stringify(maybe_wallet_info.result, undefined, 4);
 *     }
 *     else {
 *         h4.innerHTML = "error";
 *         h4.style.color = "crimson";
 *         pre.innerHTML  = JSON.stringify(maybe_wallet_info.error, undefined, 4);
 *     }
 * }
 *
 * // want to create a logger down here
 * let logger = sk.cl();
 * // detect button
 * document.getElementById('detect')!.onclick = function() { detect(logger); } ;
 * ```
 */
async function detect(timeout_ms, timeout_msg, logger) {
    let call_params = { timeout_ms: timeout_ms,
        timeout_msg: timeout_msg };
    logger.debug('detect', call_params);
    let listener = new CAPListener(logger);
    logger.debug('detect: listening on window', {});
    listener.listen();
    let result = await listener.raseev(timeout_ms, timeout_msg);
    logger.debug('detect: result', { result: result });
    logger.debug('detect: ignoring window', {});
    listener.ignore();
    return result;
}
/**
 * Listens for `connection.announcePresence` messages
 *
 * @internal
 */
class CAPListener {
    logger;
    listener;
    cap_queue = null;
    constructor(logger) {
        logger.debug('CAPListener.constructor', {});
        this.logger = logger;
        // js pointer hack
        const this_ptr = this;
        this.listener = function (event) { this_ptr.handle(event); };
    }
    listen() {
        this.logger.info('CAPListener.listen', {});
        window.addEventListener('message', this.listener);
    }
    ignore() {
        this.logger.info('CAPListener.ignore', {});
        window.removeEventListener('message', this.listener);
    }
    handle(evt) {
        this.logger.debug('CAPListener.handle', { event: evt });
        if (evt instanceof MessageEvent)
            this.really_handle(evt);
    }
    really_handle(evt) {
        this.logger.debug('CAPListener.really_handle', { event: evt });
        // Example message data:
        //
        // ```json
        // {
        //     "type": "to_aepp",
        //     "data": {
        //         "jsonrpc": "2.0",
        //         "method": "connection.announcePresence",
        //         "params": {
        //             "id": "{aee9e933-52b6-410a-8c3f-99c6be596b4e}",
        //             "name": "Superhero",
        //             "networkId": "ae_mainnet",
        //             "origin": "moz-extension://ee425d81-d5b2-44b6-9406-4da31b019e7c",
        //             "type": "extension"
        //         }
        //     }
        // }
        // ```
        //
        // Example queue data:
        //
        // ```json
        // {
        //     "id": "{aee9e933-52b6-410a-8c3f-99c6be596b4e}",
        //     "name": "Superhero",
        //     "networkId": "ae_mainnet",
        //     "origin": "moz-extension://ee425d81-d5b2-44b6-9406-4da31b019e7c",
        //     "type": "extension"
        // }
        // ```
        let queue_empty = !this.cap_queue;
        let msg_is_for_us = evt.data.type === "to_aepp";
        let is_cap_msg = evt.data.data.method === "connection.announcePresence";
        let we_rollin = queue_empty && msg_is_for_us && is_cap_msg;
        this.logger.debug('CAPListener.really_handle: branching variables regarding how to handle this event', { queue_empty: queue_empty,
            msg_is_for_us: msg_is_for_us,
            is_cap_msg: is_cap_msg,
            we_rollin: we_rollin,
            event: evt });
        if (we_rollin) {
            this.logger.debug('CAPListener.really_handle: queue empty, message is for us, and it is the message we want, so adding to queue', { event: evt,
                new_queue: evt.data.data.params });
            this.cap_queue = evt.data.data.params;
        }
        else {
            this.logger.debug("CAPListener.really_handle: for whatever reason, we're ignoring this event", { event: evt });
        }
    }
    async raseev(timeout_ms, timeout_msg) {
        this.logger.debug('CAPListener.raseev', { timeout_ms: timeout_ms,
            timeout_msg: timeout_msg });
        // stupid js pointer hack
        let this_ptr = this;
        let lambda_that_must_return_true_to_unblock = function () {
            // this means queue is not empty
            return !!(this_ptr.cap_queue);
        };
        let get_result = function () {
            this_ptr.logger.debug('CAPListener.raseev.get_result', {});
            return this_ptr.cap_queue;
        };
        let result = await bulrtot(lambda_that_must_return_true_to_unblock, get_result, timeout_ms, timeout_msg, this.logger);
        return result;
    }
}
//-----------------------------------------------------------------------------
// API: connection
//-----------------------------------------------------------------------------
/**
 * Connect to the wallet. This is a necessary step if you want {@link address}
 * to work. You **must** wait for Superhero to announce itself (see {@link
 * detect}) before attempting to connect. This has the same return data as
 * {@link detect}. This does not pop up the confirmation dialog for the user.
 * Superhero never rejects the connection request. This call is instantaneous
 * in practice.
 *
 * @example
 * ```ts
 * // document has a button with id=connect
 * // this is the function that is triggered when the button is clicked
 * // you want your user to do the detect sequence first
 * async function connect (logger: sk.Logger) : Promise<void> {
 *     let h4 = document.getElementById("connected")!;
 *     let pre = document.getElementById("connect-info")!;
 *
 *     h4.innerHTML = 'connecting...';
 *     h4.style.color = 'GoldenRod';
 *
 *     // try to connect to the wallet
 *     // will fail on timeout error
 *     let maybe_wallet_info = await sk.connect(
 *             'ske-connect-1',
 *             {name: 'sidekick examples',
 *              version: 1},
 *             sk.TIMEOUT_DEF_CONNECT_MS,
 *             "failed to connect to wallet",
 *             logger
 *         );
 *
 *     console.log(maybe_wallet_info);
 *
 *     // ok means wallet was connected
 *     if (maybe_wallet_info.ok)
 *     {
 *         h4.innerHTML   = "connected";
 *         h4.style.color = "green";
 *         pre.innerHTML  = JSON.stringify(maybe_wallet_info.result, undefined, 4);
 *     }
 *     else
 *     {
 *         h4.innerHTML = "error";
 *         h4.style.color = "crimson";
 *         pre.innerHTML  = JSON.stringify(maybe_wallet_info.error, undefined, 4);
 *     }
 * }
 *
 * let logger = sk.cl();
 * document.getElementById('connect')!.onclick = function() { connect(logger); };
 * ```
 */
async function connect(id, params, timeout_ms, timeout_msg, logger) {
    logger.debug('connect', { id: id, params: params, timeout_ms: timeout_ms, timeout_msg: timeout_msg });
    let msgr = new MsgR(logger);
    // FIXME: for type purposes, making the correct RPC message should be up here
    // this way we can enforce that it's a correct RPC call with typescript
    // Ideal:
    // let result = await msgr.send_raseev(id, awcp.METHOD_CONNECTION_OPEN, params, target, timeout_ms, timeout_msg);
    let result = await msgr.send_raseev(id, "connection.open", params, timeout_ms, timeout_msg);
    return result;
}
//-----------------------------------------------------------------------------
// API: get address
//-----------------------------------------------------------------------------
/**
 *
 */
async function address(id, params, timeout_ms, timeout_msg, logger) {
    logger.debug('address', { id: id, params: params, timeout_ms: timeout_ms, timeout_msg: timeout_msg });
    let msgr = new MsgR(logger);
    // FIXME: for type purposes, making the correct RPC message should be up here
    // this way we can enforce that it's a correct RPC call with typescript
    // Ideal:
    // let result = await msgr.send_raseev(id, awcp.METHOD_CONNECTION_OPEN, params, target, timeout_ms, timeout_msg);
    let result = await msgr.send_raseev(id, "address.subscribe", params, timeout_ms, timeout_msg);
    return result;
}
//-----------------------------------------------------------------------------
// API: message sign
//-----------------------------------------------------------------------------
/**
 * Sign the given message, return the **HASHED AND SALTED** message signature
 * (see IMPORTANT CAVEAT section), encoded in hexadecimal
 *
 * ## IMPORTANT CAVEAT: HASHING AND SALTING
 *
 * In order to exclude the possibility of someone using this functionality to
 * trick the user into signing a transaction, the wallet salts and hashes the
 * message, and *then* signs the salted/hashed message.
 *
 * Therefore, naively attempting to verify the signature will not work. You
 * must apply the same preprocessing steps as the wallet, **THEN** check the
 * signature against the salted/hashed message.
 *
 * ```erlang
 * -spec hashed_salted_msg(Message) -> HashedSaltedMessage
 *     when Message             :: binary(),
 *          HashedSaltedMessage :: binary().
 * % @doc Salt the message then hash with blake2b. See:
 * % 1. https://github.com/aeternity/aepp-sdk-js/blob/370f1e30064ad0239ba59931908d9aba0a2e86b6/src/utils/crypto.ts#L83-L85
 * % 2. https://github.com/aeternity/eblake2/blob/60a079f00d72d1bfcc25de8e6996d28f912db3fd/src/eblake2.erl#L23-L25
 *
 * hashed_salted_msg(Msg) ->
 *     {ok, HSMsg} = eblake2:blake2b(32, salted_msg(Msg)),
 *     HSMsg.
 *
 *
 *
 * -spec salted_msg(Message) -> SaltedMessage
 *     when Message       :: binary(),
 *          SaltedMessage :: binary().
 * % @doc Salt the message the way Superhero does before signing.
 * %
 * % See: https://github.com/aeternity/aepp-sdk-js/blob/370f1e30064ad0239ba59931908d9aba0a2e86b6/src/utils/crypto.ts#L171-L175
 *
 * salted_msg(Msg) when is_binary(Msg) ->
 *     P = <<"aeternity Signed Message:\n">>,
 *     {ok, SP}   = btc_varuint_encode(byte_size(P)),
 *     {ok, SMsg} = btc_varuint_encode(byte_size(Msg)),
 *     <<SP/binary,
 *       P/binary,
 *       SMsg/binary,
 *       Msg/binary>>.
 *
 *
 *
 * -spec btc_varuint_encode(Integer) -> Result
 *     when Integer :: integer(),
 *          Result  :: {ok, Encoded :: binary()}
 *                   | {error, Reason :: term()}.
 * % @doc Bitcoin varuint encode
 * %
 * % See: https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer
 *
 * btc_varuint_encode(N) when N < 0 ->
 *     {error, {negative_N, N}};
 * btc_varuint_encode(N) when N < 16#FD ->
 *     {ok, <<N>>};
 * btc_varuint_encode(N) when N =< 16#FFFF ->
 *     NBytes = eu(N, 2),
 *     {ok, <<16#FD, NBytes/binary>>};
 * btc_varuint_encode(N) when N =< 16#FFFF_FFFF ->
 *     NBytes = eu(N, 4),
 *     {ok, <<16#FE, NBytes/binary>>};
 * btc_varuint_encode(N) when N < (2 bsl 64) ->
 *     NBytes = eu(N, 8),
 *     {ok, <<16#FF, NBytes/binary>>}.
 *
 * % eu = encode unsigned (little endian with a given byte width)
 * % means add zero bytes to the end as needed
 * eu(N, Size) ->
 *     Bytes = binary:encode_unsigned(N, little),
 *     NExtraZeros = Size - byte_size(Bytes),
 *     ExtraZeros = << <<0>> || _ <- lists:seq(1, NExtraZeros) >>,
 *     <<Bytes/binary, ExtraZeros/binary>>.
 * ```
 *
 * @example
 * This function is triggered when a "sign message" button is pressed. A text
 * input with `id="message-text"` contains the message to be signed.
 * ```ts
 * async function sign_msg (logger: sk.Logger) : Promise<void> {
 *     let acc_pubkey : string = pv_address;
 *     // @ts-ignore value property exists because i say so
 *     let msg_text   : string = document.getElementById('message-text').value;
 *     let signed_msg = await sk.msg_sign('sk-msg-sign-1', acc_pubkey, msg_text, sk.TIMEOUT_DEF_MSG_SIGN_MS, 'message signing took too long', logger);
 *     console.log('signed message:', signed_msg);
 * }
 * ```
 */
async function msg_sign(id, account_pubkey, message, timeout_ms, timeout_msg, logger) {
    logger.debug('msg_sign', { id: id, account_pubkey: account_pubkey, message: message, timeout_ms: timeout_ms, timeout_msg: timeout_msg });
    let msgr = new MsgR(logger);
    // FIXME: for type purposes, making the correct RPC message should be up here
    // this way we can enforce that it's a correct RPC call with typescript
    // Ideal:
    // let result = await msgr.send_raseev(id, awcp.METHOD_CONNECTION_OPEN, params, target, timeout_ms, timeout_msg);
    let result = await msgr.send_raseev(id, "message.sign", { onAccount: account_pubkey, message: message }, timeout_ms, timeout_msg);
    return result;
}
//-----------------------------------------------------------------------------
// API: tx sign (no prop)
//-----------------------------------------------------------------------------
async function tx_sign_noprop(id, params, timeout_ms, timeout_msg, logger) {
    logger.debug('address', { id: id, params: params, timeout_ms: timeout_ms, timeout_msg: timeout_msg });
    let msgr = new MsgR(logger);
    // FIXME: for type purposes, making the correct RPC message should be up here
    // this way we can enforce that it's a correct RPC call with typescript
    // Ideal:
    // let result = await msgr.send_raseev(id, awcp.METHOD_CONNECTION_OPEN, params, target, timeout_ms, timeout_msg);
    let result = await msgr.send_raseev(id, "transaction.sign", params, timeout_ms, timeout_msg);
    return result;
}
class MsgR {
    logger;
    listener;
    // FIXDME: make this a map
    queue = new Map();
    constructor(logger) {
        this.logger = logger;
        this.logger.debug('MsgR.constructor', {});
        const this_ptr = this;
        this.listener = function (event) { this_ptr.handle(event); };
    }
    listen() {
        this.logger.debug('MsgR.listen', {});
        window.addEventListener('message', this.listener);
    }
    ignore() {
        this.logger.debug('MsgR.ignore', {});
        window.removeEventListener('message', this.listener);
    }
    handle(evt) {
        this.logger.debug('MsgR.handle', { event: evt });
        if (evt instanceof MessageEvent)
            this.really_handle(evt);
    }
    really_handle(evt) {
        this.logger.debug('MsgR.really_handle', { event: evt });
        // message is
        //
        // raseeving based on id
        //
        // {
        //     "type": "to_aepp",
        //     "data": {
        //         "jsonrpc": "2.0",
        //         "method": don't care,
        //         "id": the key
        //     }
        // }
        //
        // value is the entire message data
        // is it for us, and does it have an id field
        if ((evt.data.type === "to_aepp") && !!(evt.data.data.id)) {
            // now we know it's a message for us
            let key = evt.data.data.id;
            let val = evt.data;
            this.queue.set(key, val);
        }
    }
    async send_raseev(id, method, params, timeout_ms, timeout_msg) {
        this.logger.debug('MsgR.send_and_raseev', { id: id,
            method: method,
            params: params,
            timeout_ms: timeout_ms,
            timeout_msg: timeout_msg });
        // make the message
        let window_msg = mk_window_msg(id, method, params);
        this.logger.debug('MsgR.send_and_raseev posting message', { window_msg: window_msg });
        // listen
        this.listen();
        // send the message
        window.postMessage(window_msg);
        // receive reply
        let response = await this.raseev(id, timeout_ms, timeout_msg);
        // unwrap the rpc jizz
        let result = Safe_AWCP_W2A_Msg_to_Safe_result(response);
        // ignore target
        this.ignore();
        return result;
    }
    async raseev(id, timeout_ms, timeout_msg) {
        this.logger.debug('MsgQ.raseev', { id: id, timeout_ms: timeout_ms, timeout_msg: timeout_msg });
        // js pointer hack
        let this_ptr = this;
        let lambda_that_must_return_true_to_unblock = function () { return this_ptr.queue.has(id); };
        let result_fun = function () { return this_ptr.queue.get(id); };
        let result = await bulrtot(lambda_that_must_return_true_to_unblock, result_fun, timeout_ms, timeout_msg, this.logger);
        return result;
    }
}
function mk_window_msg(id, method, params) {
    let rpc_message = mk_rpc_message(id, method, params);
    return { type: "to_waellet",
        data: rpc_message };
}
function mk_rpc_message(id, method, params) {
    return { jsonrpc: "2.0",
        id: id,
        method: method,
        params: params };
}
///**
// * Returns the value of `dispatchEvent`
// *
// * See https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/dispatchEvent
// */
//function
//send
//    <data_t extends any>
//    (tgt  : EventTarget & MessageEventSource,
//     data : data_t)
//    : boolean
//{
//    // TODO: look at options besides `data`
//    // See: https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent/MessageEvent
//    let msg_event: MessageEvent<data_t> = new MessageEvent('message', {data: data, source: tgt});
//    let result = tgt.dispatchEvent(msg_event);
//    return result;
//}
//
//-----------------------------------------------------------------------------
// INTERNALS
//-----------------------------------------------------------------------------
/**
 * Stack overflow: https://stackoverflow.com/questions/951021/what-is-the-javascript-version-of-sleep
 *
 * No fucking idea what's going on here
 *
 * Some crazy async hack bullshit
 *
 * It works, who cares
 *
 * @internal
 */
async function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}
/**
 * Block until lambda returns true or timeout
 *
 * `timeout_ms` should be divisible by `50`
 *
 * @internal
 */
async function bulrtot(fun, result_fun, timeout_ms, timeout_msg, logger) {
    logger.debug('bulrtot (block until lambda returns true or timeout)', { timeout_ms: timeout_ms,
        timeout_msg: timeout_msg });
    let max_iters = Math.floor(timeout_ms / 50);
    logger.debug('bulrtot: iterating every 50 milliseconds', { max_iters: max_iters });
    for (let i = 1; i <= max_iters; i++) {
        if (fun()) {
            logger.debug('bulrtot: lambda returned true on i-th iteration', { i: i, max_iters: max_iters });
            let result = ok(result_fun());
            logger.debug('bulrtot: result (ok)', { result: result });
            return result;
        }
        else
            await sleep(50);
    }
    logger.debug('bulrtot: max iterations exceeded', { max_iters: max_iters });
    let result = sk_timeout(timeout_msg, {});
    logger.debug('bulrtot: result (error)', { result: result });
    return error(result);
}
/**
 * Converts a `Safe`-wrapped `RpcResp` (which may be a success/error) to a
 * `Safe` value
 *
 * Errors can be generated from one of two places: from the wallet (which
 * encodes it in the RPC response), or from sidekick via a timeout.
 *
 * Basically, when we call `bulrtot`, we're going to get back a `Safe`-wrapped
 * `awcp.RpcResp`, which may be success value or an error value.
 *
 * In the timeout error case, preserve.
 *
 * In the case of RPC success, this unwraps whatever is in the `RpcResp`.
 *
 * In the case of RPC failure, this unwraps the error.
 *
 * @internal
 */
function Safe_AWCP_W2A_Msg_to_Safe_result(safe_w2a_msg) {
    // if input is a success (i.e. NOT a Timeout error), branch on if it's an rpc
    // error
    if (safe_w2a_msg.ok) {
        // we have 
        // {ok: true, result: {type: "to_aepp", data: rpc jizz}}
        // want to pull out the rpc jizz
        // case split on whether or not there was an RPC error (i.e. an error generated by the wallet)
        // then our top level return is a safety-wrapped error
        // which is either {ok, TheActualResultWeWant} or {error, RpcError}
        // (this branch of the if) or {error, TimeoutError} (the else branch)
        let ok_w2a_msg = safe_w2a_msg.result;
        let rpc_resp = ok_w2a_msg.data;
        // From AWCP:
        // /**
        //  * This is the shape of unsuccessful responses
        //  */
        // type RpcResp_error
        //     <method_s extends string>
        //     = {jsonrpc : "2.0",
        //        id      : number | string,
        //        method  : method_s,
        //        error   : RpcError};
        // /**
        //  * This is the shape of successful responses
        //  */
        // type RpcResp_ok
        //     <method_s extends string,
        //      result_t extends any>
        //     = {jsonrpc : "2.0",
        //        id      : number | string,
        //        method  : method_s,
        //        result  : result_t};
        // /**
        //  * This is the shape of generic responses
        //  */
        // type RpcResp
        //     <method_s extends string,
        //      result_t extends any>
        //     = RpcResp_ok<method_s, result_t>
        //     | RpcResp_error<method_s>;
        // so this may be an RpcResp_ok or an RpcResp_error
        // the next line figures that out
        // @ts-ignore typescript is mad because the property that i'm testing to see if it exists might not exist
        let rpc_resp_is_ok = !!(rpc_resp.result);
        // branch on if we're an RpcResp_ok or an RpcResp_error
        if (rpc_resp_is_ok) {
            // ok so here we're in the RpcResp_ok branch
            // the `result` field exists and we have it
            let the_actual_result = rpc_resp.result;
            return ok(the_actual_result);
        }
        // error case:
        else {
            let the_error = rpc_resp.error;
            return error(the_error);
        }
    }
    // this is the timeout error case
    //
    // in which case, the result is what we want
    else {
        return safe_w2a_msg;
    }
}
//# sourceMappingURL=sidekick.js.map