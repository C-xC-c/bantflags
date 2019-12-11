// ==UserScript==
// @name        BantFlags
// @namespace   BintFlegs
// @description More flags for r/banter
// @include     http*://boards.4chan.org/bant/*
// @include     http*://archive.nyafuu.org/bant/*
// @include     http*://archived.moe/bant/*
// @include     http*://thebarchive.com/bant/*
// @exclude     http*://boards.4chan.org/bant/catalog
// @exclude     http*://archive.nyafuu.org/bant/statistics/
// @exclude     http*://archived.moe/bant/statistics/
// @exclude     http*://thebarchive.com/bant/statistics/
// @version     1.0.0
// @grant       GM_xmlhttpRequest
// @grant       GM_getValue
// @grant       GM_setValue
// @run-at      document-end
// @icon        https://nineball.party/files/flags/actual_flags/0077.png
// @updateURL    https://flags.plum.moe/bantflags.meta.js
// @downloadURL  https://flags.plum.moe/bantflags.user.js
// ==/UserScript==

// This script specifically targets ECMAScript 2015 (const, let, arrow functions). Update your hecking browser.

// Change this if you want verbose debug information in the console.
const debugMode = true;

//
// DO NOT EDIT ANYTHING IN THIS SCRIPT DIRECTLY - YOUR FLAGS SHOULD BE CONFIGURED USING THE CONFIGURATION BOXES
//
const postRemoveCounter = 60;
const requestRetryInterval = 5000; // TODO: maybe a max retries counter?
const regionDivider = "||"; //TODO: We can probably remove this and seperate by ,
const is_archive = window.location.host !== "boards.4chan.org";
const boardID = "bant"; //TODO: Hardcode /bant/ or accept other boards.
const version = 2; // Breaking changes.
const back_end = 'https://localhost:44366/';
const api_flags = 'api/flags';
const flag_dir = 'flags/';
const api_get = 'api/get';
const api_post = 'api/post';

// If you increase this the server will ignore your post.
const max_flags = 24;

var regions = []; // The flags we have selected.
var postNrs = []; // all post numbers in the thread.

//
// DO NOT EDIT ANYTHING IN THIS SCRIPT DIRECTLY - YOUR FLAGS SHOULD BE CONFIGURED USING THE CONFIGURATION BOXES
//

let elementsInClass = x => document.getElementsByClassName(x);
let sliceCall = x => Array.prototype.slice.call(x);
let firstChildInClass = (parent, className) => parent.getElementsByClassName(className)[0];
let createAndAssign = (element, source) => Object.assign(document.createElement(element), source);

function addGlobalStyle(css) {
    let head = document.getElementsByTagName('head')[0];
    if (!head) {
        console.error('[BantFlags] No head tag??');
        return;
    }

    head.appendChild(createAndAssign('style', {
        type: 'text/css',
        innerHTML: css
    }));
}

function debug(text) {
    if (debugMode) {
        console.log("[BantFlags] " + text);
    }
}

/** Wrapper around GM_xmlhttpRequest
 * @param {string} method - The HTTP method (GET, POST)
 * @param {string} url - The URL of the request
 * @param {string} data - Data sent inn the form body
 * @param {Function} func - The function run after onload. Response data is sent directly to it. */
function MakeRequest(method, url, data, func) {
    GM_xmlhttpRequest({
        method: method,
        url: url,
        data: data,
        headers: {
            "Content-Type": "application/x-www-form-urlencoded"
        },
        onload: func
    });
}

function retry(func, resp) {
    console.log("[BantFlags] Could not fetch flags, status: " + resp.status);
    console.log(resp.statusText); // TODO: surely ASP.NET can return something more useful?
    setTimeout(func, requestRetryInterval);
}

/** nSetup, preferences */

// TODO: this shouldn't be a class.
var nsetup = { // not anymore a clone of the original setup
    namespace: 'BintFlegs', // TODO: should be const.
    flagsLoaded: false,
    form: "<span id=\"bantflags_container\"></span>" +
        "<button type=\"button\" id=\"append_flag_button\" title=\"Click to add selected flag to your flags. Click on flags to remove them. Saving happens automatically, you only need to refresh the pages that have an outdated flaglist on the page.\"><<</button>" +
        "<button id=\"flagLoad\" type=\"button\">Click to load flags.</button><select id=\"flagSelect\"></select>",
    fillHtml: function () { // TODO: this function should have a better name. Only called by nsetup.init, can be inlined?

        // resolve flags
        MakeRequest(
            "GET",
            back_end + api_flags,
            "version=" + encodeURIComponent(version),
            function (resp) {
                debug('Loading flags');
                if (resp.status !== 200) {
                    retry(nsetup.fillHtml, resp);
                    return;
                }

                let flagSelect = document.getElementById("flagSelect");
                let flagLoad = document.getElementById('flagLoad');
                let flagsSupported = resp.responseText.split('\n');

                for (var i = 0; i < flagsSupported.length; i++) {
                    let flag = flagsSupported[i];
                    flagSelect.appendChild(createAndAssign('option', {
                        value: flag,
                        innerHTML: "<img src=\"" + back_end + flag_dir + flag + ".png\"" + " title=\"" + flag + "\">" + " " + flag
                    }));
                }

                flagLoad.style.display = 'none';
                flagSelect.style.display = 'inline-block';
                nsetup.flagsLoaded = true;
                flagLoad.removeEventListener('click', nsetup.fillHtml);
            });
    },
    save: function (v) {
        GM_setValue(nsetup.namespace, v);
        regions = GM_getValue(nsetup.namespace);
    },
    setFlag: function (flag) { // place a flag from the selector to the flags array variable and create an element in the flags_container div
        let UID = Math.random().toString(36).substring(7);
        let flagName = flag ? flag : document.getElementById("flagSelect").value;
        let flagContainer = document.getElementById("bantflags_container");

        flagContainer.appendChild(createAndAssign('img', {
            title: flagName,
            src: back_end + flag_dir + flagName + ".png",
            id: UID,
            className: 'bantflags_flag'
        }));

        if (flagContainer.children.length > max_flags) {
            nsetup.ToggleFlagButton('off');
        }

        document.getElementById(UID).addEventListener("click", function () {
            let flagToRemove = document.getElementById(UID);

            flagToRemove.parentNode.removeChild(flagToRemove);
            nsetup.toggleFlagButton('on');
            nsetup.save(nsetup.parse());
        });

        if (!flag) {
            nsetup.save(nsetup.parse());
        }
    },

    init: function () {

        // here we insert the form for placing flags. How?
        let flagsForm = createAndAssign("div", {
            className: 'flagsForm',
            innerHTML: nsetup.form
        });

        addGlobalStyle('.flagsForm{float: right; clear: right; margin: 20px 10px;} #flagSelect{display:none;}');
        addGlobalStyle(".bantflags_flag { padding: 1px;} [title^='Romania'] { position: relative; animation: shakeAnim 0.1s linear infinite;} @keyframes shakeAnim { 0% {left: 1px;} 25% {top: 2px;} 50% {left: 1px;} 75% {left: 0px;} 100% {left: 2px;}}");

        firstChildInClass(document, 'bottomCtrl').parentNode.appendChild(flagsForm);

        for (var i in regions) {
            nsetup.setFlag(regions[i]);
        }

        document.getElementById('append_flag_button').addEventListener('click',
            () => nsetup.flagsLoaded ? nsetup.setFlag() : alert('Load flags before adding them.'));

        document.getElementById('flagLoad').addEventListener('click', nsetup.fillHtml);
    },
    parse: function () {
        let flagsArray = [];
        let flagElements = elementsInClass("bantflags_flag");

        for (var i = 0; i < flagElements.length; i++) {
            flagsArray[i] = flagElements[i].title;
        }

        return flagsArray;
    },
    ToggleFlagButton: state => document.getElementById('append_flag_button').disabled = state === 'off' ? true : false
};

/** Prompt to set region if regions is empty  */
regions = GM_getValue(nsetup.namespace); // TODO: move this to other init stuff
if (!regions) {
    regions = [];
    setTimeout(function () {
        window.confirm("Bant Flags: No Flags detected");
    }, 2000);
}

/** parse the posts already on the page before thread updater kicks in */
function parse4chanPosts() {
    let posts = sliceCall(elementsInClass('postContainer'));

    for (var i = 0; i < posts.length; i++) {
        let postNumber = posts[i].id.replace("pc", "");
        postNrs.push(postNumber);
    }
    debug(postNrs);
}

function parseFoolFuukaPosts() {
    let nums = x => x.filter(x => x.id !== '').map(x => x.id);
    let getPostNumbers = x => nums(sliceCall(elementsInClass(x)));

    postNrs = getPostNumbers('thread').concat(getPostNumbers('post'));
    debug(postNrs);
}

function onFlagsLoad(response) {

    // because we only care about the end result, not how we got there.
    // grandparent -> first parent -> first child.
    let hopHTML = (post_nr, first, second) =>
        firstChildInClass(firstChildInClass(document.getElementById(post_nr), first), second);

    let MakeFlag = (flag) =>
        createAndAssign('a', {
            innerHTML: "<img src=\"" + back_end + flag_dir + flag + ".png\" title=\"" + flag + "\">",
            className: "bantFlag",
            target: "_blank"
        });

    debug("JSON: " + response.responseText);
    var jsonData = JSON.parse(response.responseText);

    Object.keys(jsonData).forEach(function (post) {
        let flagContainer = is_archive
            ? hopHTML(post, "post_data", "post_type")
            : hopHTML("pc" + post, "postInfo", "nameBlock");
        let currentFlag = firstChildInClass(flagContainer, 'flag');
        let flags = jsonData[post];

        // If we have a bantflag and the original post has a flag
        if (flags.length > 0 && currentFlag !== undefined) {
            console.log("[BantFlags] Resolving flags for >>" + post);

            for (var i = 0; i < flags.length; i++) {
                let flag = flags[i];

                let newFlag = MakeFlag(flag);
                if (is_archive) {
                    newFlag.style = "padding: 0px 0px 0px " + (3 + 2 * (i > 0)) + "px; vertical-align:;display: inline-block; width: 16px; height: 11px; position: relative;";
                }

                flagContainer.append(newFlag);

                console.log("\t -> " + flag);
            }
        }
    });

    postNrs = [];
}

function resolveRefFlags() {
    MakeRequest(
        "POST",
        back_end + api_get,
        "post_nrs=" + encodeURIComponent(postNrs) + "&board=" + encodeURIComponent(boardID) + "&version=" + encodeURIComponent(version),
        function (resp) {
            if (resp.status !== 200) {
                retry(resolveRefFlags, resp);
                return;
            }
            onFlagsLoad(resp);
        }
    );
}

// Flags need to be parsed and aligned differently between boards.
if (is_archive) {
    debug("FoolFuuka.");
    parseFoolFuukaPosts();

    addGlobalStyle('.bantFlag{top: -2px !important;left: -1px !important}');
}
else {
    debug("4chan.");
    parse4chanPosts();

    addGlobalStyle('.flag{top: 0px !important;left: -1px !important}');
    addGlobalStyle(".bantFlag {padding: 0px 0px 0px 5px; vertical-align:;display: inline-block; width: 16px; height: 11px; position: relative;}");
}

resolveRefFlags(); // Get flags from db.

if (!is_archive) {
    let GetEvDetail = e => e.detail || e.wrappedJSObject.detail;

    let method = "POST",
        url = back_end + api_post,
        func = function (resp) {
            debug(resp.responseText);
        };

    // TODO: most of this can be rewritten. Do we care to support GM 1.x when we're using ES6?
    /** send flag to system on 4chan x (v2, loadletter, v3 untested) post
     *  handy comment to save by ccd0
     *  console.log(e.detail.boardID);  // board name    (string)
     *  console.log(e.detail.threadID); // thread number (integer in ccd0, string in loadletter)
     *  console.log(e.detail.postID);   // post number   (integer in ccd0, string in loadletter) */
    document.addEventListener('QRPostSuccessful', function (e) {

        //setTimeout to support greasemonkey 1.x
        setTimeout(function () {
            var data = "post_nr=" + encodeURIComponent(e.detail.postID) + "&board=" + encodeURIComponent(e.detail.boardID) + "&regions=" + encodeURIComponent(regions) + "&version=" + encodeURIComponent(version);
            MakeRequest(method, url, data, func);
        }, 0);
    }, false);

    /** send flag to system on 4chan inline post */
    document.addEventListener('4chanQRPostSuccess', function (e) {
        var evDetail = GetEvDetail(e);

        //setTimeout to support greasemonkey 1.x
        setTimeout(function () {
            var data = "post_nr=" + encodeURIComponent(evDetail.postId) + "&board=" + encodeURIComponent(boardID) + "&regions=" + encodeURIComponent(regions) + "&version=" + encodeURIComponent(version);
            MakeRequest(method, url, data, func);
        }, 0);
    }, false);

    /** Listen to post updates from the thread updater for 4chan x v2 (loadletter) and v3 (ccd0 + ?) */
    document.addEventListener('ThreadUpdate', function (e) {
        var evDetail = GetEvDetail(e);
        var evDetailClone = typeof cloneInto === 'function' ? cloneInto(evDetail, unsafeWindow) : evDetail;

        //ignore if 404 event
        if (evDetail[404] === true) {
            return;
        }

        setTimeout(function () {

            //add to temp posts and the DOM element to allPostsOnPage
            evDetailClone.newPosts.forEach(function (post_board_nr) {
                var post_nr = post_board_nr.split('.')[1];
                postNrs.push(post_nr);
            });
        }, 0);

        //setTimeout to support greasemonkey 1.x
        setTimeout(resolveRefFlags, 0);
    }, false);

    /** Listen to post updates from the thread updater for inline extension */
    document.addEventListener('4chanThreadUpdated', function (e) {
        var evDetail = GetEvDetail(e);
        let threadID = window.location.pathname.split('/')[3];
        let postsContainer = sliceCall(document.getElementById('t' + threadID).childNodes);
        let lastPosts = postsContainer.slice(Math.max(postsContainer.length - evDetail.count, 1)); //get the last n elements (where n is evDetail.count)

        //add to temp posts and the DOM element to allPostsOnPage
        lastPosts.forEach(function (post_container) {
            var post_nr = post_container.id.replace("pc", "");
            postNrs.push(post_nr);
        });

        //setTimeout to support greasemonkey 1.x
        setTimeout(resolveRefFlags, 0);
    }, false);
    /** setup init and start first calls */
    nsetup.init();
}