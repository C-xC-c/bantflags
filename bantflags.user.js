// ==UserScript==
// @name        BantFlags
// @namespace   BintFlegs
// @description More flags for r/banter
// @include     http*://boards.4chan.org/bant/*
// @include     http*://archive.nyafuu.org/bant/*
// @include     http*://archived.moe/bant/*
// @include     http*://thebarchive.com/bant/*
// @include     http*://nineball.party/*
// @exclude     http*://boards.4chan.org/bant/catalog
// @exclude     http*://archive.nyafuu.org/bant/statistics/
// @exclude     http*://archived.moe/bant/statistics/
// @exclude     http*://thebarchive.com/bant/statistics/
// @version     1.2.0
// @grant       GM_xmlhttpRequest
// @grant       GM_getValue
// @grant       GM_setValue
// @run-at      document-end
// @icon        https://nineball.party/files/flags/actual_flags/0077.png
// @updateURL    https://flags.plum.moe/bantflags.meta.js
// @downloadURL  https://flags.plum.moe/bantflags.user.js
// ==/UserScript==

// (C) Copyright 2019 C-xC-c <boku@plum.moe>
// This file is part of BantFlags.
// BantFlags is licensed under the GNU AGPL Version 3.0 or later.
// see the LICENSE file or <https://www.gnu.org/licenses/>

// This script specifically targets ECMAScript 2015 (const, let, arrow functions). Update your hecking browser.

// Change this if you want verbose debuging information in the console.
const debugMode = false;

//
// DO NOT EDIT ANYTHING IN THIS SCRIPT DIRECTLY - YOUR FLAGS SHOULD BE CONFIGURED USING THE CONFIGURATION BOXES
//
const postRemoveCounter = 60;
const requestRetryInterval = 5000; // TODO: maybe a max retries counter as well?
const version = 2; // Breaking changes.
const back_end = 'https://flags.plum.moe/';
const api_flags = 'api/flags';
const flag_dir = 'flags/';
const api_get = 'api/get';
const api_post = 'api/post';

// If you increase this the server will ignore your post.
const max_flags = 30;

var regions = []; // The flags we have selected.
var postNrs = []; // all post numbers in the thread.
var board_id = ""; // The board we get flags for.

// Test unqiue CSS paths to figure out what board software we're using.
const software = {
    yotsuba: window.location.host === 'boards.4chan.org',
    gogucaDoushio: document.querySelector('section article[id] header .control') !== null,
    foolfuuka: document.querySelector('div[id="main"] article header .post_data') !== null
};

//
// DO NOT EDIT ANYTHING IN THIS SCRIPT DIRECTLY - YOUR FLAGS SHOULD BE CONFIGURED USING THE CONFIGURATION BOXES
//
const sliceCall = x => Array.prototype.slice.call(x);
const createAndAssign = (element, source) => Object.assign(document.createElement(element), source);

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
        console.log('[BantFlags] ' + text);
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
            "Content-Type": 'application/x-www-form-urlencoded'
        },
        onload: func
    });
}

function retry(func, resp) {
    console.log('[BantFlags] Could not fetch flags, status: ' + resp.status);
    console.log(resp.statusText);
    setTimeout(func, requestRetryInterval);
}

/** nSetup, preferences */

// TODO: this shouldn't be a object.
var nsetup = { // not anymore a clone of the original setup
    namespace: 'BintFlegs', // TODO: should be const.
    flagsLoaded: false,
    form: '<span id="bantflags_container"></span><button type="button" id="append_flag_button" title="Click to add selected flag to your flags. Click on flags to remove them. Saving happens automatically, you only need to refresh the pages that have an outdated flaglist on the page."><<</button><button id="flagLoad" type="button">Click to load flags.</button><select id="flagSelect"></select>',
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

                let flagSelect = document.getElementById('flagSelect');
                let flagLoad = document.getElementById('flagLoad');
                let flagsSupported = resp.responseText.split('\n');

                for (var i = 0; i < flagsSupported.length; i++) {
                    let flag = flagsSupported[i];
                    flagSelect.appendChild(createAndAssign('option', {
                        value: flag,
                        innerHTML: '<img src="' + back_end + flag_dir + flag + '.png" title="' + flag + '"> ' + flag
                    }));
                }

                flagLoad.style.display = 'none';
                flagSelect.style.display = 'inline-block';
                nsetup.flagsLoaded = true;
            });
    },
    save: function (v) {
        GM_setValue(nsetup.namespace, v);
        regions = GM_getValue(nsetup.namespace);
    },
    setFlag: function (flag) {
        let UID = Math.random().toString(36).substring(7);
        let flagName = flag ? flag : document.getElementById('flagSelect').value;
        let flagContainer = document.getElementById('bantflags_container');

        flagContainer.appendChild(createAndAssign('img', {
            title: flagName,
            src: back_end + flag_dir + flagName + '.png',
            id: UID,
            className: 'bantflags_flag'
        }));

        if (flagContainer.children.length >= max_flags) {
            nsetup.toggleFlagButton('off');
        }

        document.getElementById(UID).addEventListener("click", function (e) {
            flagContainer.removeChild(e.target);
            nsetup.toggleFlagButton('on');
            nsetup.save(nsetup.parse());
        });

        if (!flag) {
            nsetup.save(nsetup.parse());
        }
    },

    init: function () {
        let flagsForm = createAndAssign('div', {
            className: 'flagsForm',
            innerHTML: nsetup.form
        });

        // Where do we append the flagsForm to?
        if (software.yotsuba) { document.getElementById('delform').appendChild(flagsForm); }
        if (software.gogucaDoushio) { document.querySelector('section').append(flagsForm); }

        for (var i in regions) {
            nsetup.setFlag(regions[i]);
        }

        document.getElementById('append_flag_button').addEventListener('click',
            () => nsetup.flagsLoaded ? nsetup.setFlag() : alert('Load flags before adding them.'));

        document.getElementById('flagLoad').addEventListener('click', nsetup.fillHtml, { once: true });
    },
    parse: function () {
        let flagsArray = [];
        let flagElements = document.getElementsByClassName("bantflags_flag");

        for (var i = 0; i < flagElements.length; i++) {
            flagsArray[i] = flagElements[i].title;
        }

        return flagsArray;
    },
    toggleFlagButton: state => document.getElementById('append_flag_button').disabled = state === 'off' ? true : false
};

/** Prompt to set region if regions is empty  */
regions = GM_getValue(nsetup.namespace); // TODO: move this to other init stuff
if (!regions) {
    regions = [];
    setTimeout(function () {
        window.confirm('Bant Flags: No Flags detected');
    }, 2000);
}

/** parse the posts already on the page before thread updater kicks in */
function parse4chanPosts() {
    let posts = document.querySelectorAll('.postContainer');
    for (var i = 0; i < posts.length; i++) {
        let postNumber = posts[i].id.replace('pc', ''); // Fuck you 4chan
        postNrs.push(postNumber);
    }
    debug(postNrs);
}

function getposts(selector) {
    let posts = document.querySelectorAll(selector);

    for (var i = 0; i < posts.length; i++) {
        let postNumber = software.yotsuba
            ? posts[i].id.replace('pc', '') // Fuck you 4chan.
            : posts[i].id;
        postNrs.push(posts[i].id);
    }
    debug(postNrs);
}

function onFlagsLoad(response) {
    let MakeFlag = (flag) =>
        createAndAssign('a', {
            innerHTML: '<img src="' + back_end + flag_dir + flag + '.png" title="' + flag + '"> ',
            className: 'bantFlag',
            target: '_blank'
        });

    debug('JSON: ' + response.responseText);
    var jsonData = JSON.parse(response.responseText);

    Object.keys(jsonData).forEach(function (post) {

        // Here we get the header of the post using a different CSS selector depending on the board.
        var flagContainer;
        if (software.gogucaDoushio) { flagContainer = document.querySelector('[id="' + post + '"] header'); }
        if (software.yotsuba) { flagContainer = document.querySelector('[id="pc' + post + '"] .postInfo  .nameBlock'); }
        if (software.foolfuuka) { flagContainer = document.querySelector('[id="' + post + '"] .post_data .post_type'); }

        let flags = jsonData[post];
        if (flags.length > 0) {
            console.log('[BantFlags] Resolving flags for >>' + post);

            for (var i = 0; i < flags.length; i++) {
                let flag = flags[i];

                let newFlag = MakeFlag(flag);

                if (software.foolfuuka) {
                    newFlag.style = 'padding: 0px 0px 0px ' + (3 + 2 * (i > 0)) + 'px; vertical-align:;display: inline-block; width: 16px; height: 11px; position: relative;';
                }

                if (software.gogucaDoushio) {
                    newFlag.title = flag;
                }

                flagContainer.append(newFlag);

                console.log('\t -> ' + flag);
            }
        }
    });

    postNrs = [];
}

/** Gets flags from the database. */
function resolveRefFlags() {
    debug('Board is: ' + board_id);
    MakeRequest(
        'POST',
        back_end + api_get,
        'post_nrs=' + encodeURIComponent(postNrs) + '&board=' + encodeURIComponent(board_id) + '&version=' + encodeURIComponent(version),
        function (resp) {
            if (resp.status !== 200) {
                retry(resolveRefFlags, resp);
                return;
            }
            onFlagsLoad(resp);
        }
    );
}

addGlobalStyle('.flagsForm{float: right; clear: right; margin: 20px 10px;} #flagSelect{display:none;}');
addGlobalStyle('.bantflags_flag { padding: 1px;} [title^="Romania"] { position: relative; animation: shakeAnim 0.1s linear infinite;} @keyframes shakeAnim { 0% {left: 1px;} 25% {top: 2px;} 50% {left: 1px;} 75% {left: 0px;} 100% {left: 2px;}}');

// Flags need to be parsed differently and have different styles depending on board software.
if (software.yotsuba) {
    debug('4chan');
    board_id = 'bant';
    parse4chanPosts();

    addGlobalStyle('.bantFlag {padding: 0px 0px 0px 5px; vertical-align:;display: inline-block; width: 16px; height: 11px; position: relative;} .flag{top: 0px !important;left: -1px !important}');
}

if (software.gogucaDoushio) {
    debug('Nineball');
    board_id = window.location.pathname.split('/')[1]; // 'nap' or 'srsbsn'
    getposts('section[id], article[id]');

    addGlobalStyle('.bantFlag {cursor: default} .bantFlag img {pointer-events: none;}');
}

if (software.foolfuuka) { // Archive.
    debug('FoolFuuka');
    board_id = 'bant';
    getposts('article[id]');

    addGlobalStyle('.bantFlag{top: -2px !important;left: -1px !important}');
}

resolveRefFlags(); // Get flags from DB.

// Posting new flags and getting flags as posts are added to the thread.
if (software.yotsuba) {
    let GetEvDetail = e => e.detail || e.wrappedJSObject.detail;

    let method = 'POST',
        url = back_end + api_post,
        func = function (resp) {
            debug(resp.responseText);
        };

    document.addEventListener('QRPostSuccessful', function (e) {
        var data = 'post_nr=' + encodeURIComponent(e.detail.postID) + '&board=' + encodeURIComponent(e.detail.boardID) + '&regions=' + encodeURIComponent(regions) + '&version=' + encodeURIComponent(version);
        MakeRequest(method, url, data, func);
    }, false);

    /** send flag to system on 4chan inline post */
    document.addEventListener('4chanQRPostSuccess', function (e) {
        var evDetail = GetEvDetail(e);
        var data = 'post_nr=' + encodeURIComponent(evDetail.postId) + '&board=' + encodeURIComponent(board_id) + '&regions=' + encodeURIComponent(regions) + '&version=' + encodeURIComponent(version);
        MakeRequest(method, url, data, func);
    }, false);

    /** Listen to post updates from the thread updater for 4chan x v2 (loadletter) and v3 (ccd0 + ?) */
    document.addEventListener('ThreadUpdate', function (e) {
        var evDetail = GetEvDetail(e);
        var evDetailClone = typeof cloneInto === 'function' ? cloneInto(evDetail, unsafeWindow) : evDetail;

        //ignore if 404 event
        if (evDetail[404] === true) {
            return;
        }

        evDetailClone.newPosts.forEach(function (post_board_nr) {
            var post_nr = post_board_nr.split('.')[1];
            postNrs.push(post_nr);
        });

        resolveRefFlags();
    }, false);

    /** Listen to post updates from the thread updater for inline extension */
    document.addEventListener('4chanThreadUpdated', function (e) {
        var evDetail = GetEvDetail(e);
        let threadID = window.location.pathname.split('/')[3];
        let postsContainer = sliceCall(document.getElementById('t' + threadID).childNodes);
        let lastPosts = postsContainer.slice(Math.max(postsContainer.length - evDetail.count, 1)); //get the last n elements (where n is evDetail.count)

        //add to temp posts and the DOM element to allPostsOnPage
        lastPosts.forEach(function (post_container) {
            var post_nr = post_container.id.replace('pc', '');
            postNrs.push(post_nr);
        });

        resolveRefFlags();
    }, false);

    nsetup.init();
}

if (software.gogucaDoushio) {
    nsetup.init();

    // There are some setTimeouts here since posts can appear faster than the Db can process them.
    new MutationObserver(function (mutations) {
        mutations.forEach(function (mutation) {
            if (mutation.addedNodes.length > 0) { // A post was added.
                if (mutation.target.nodeName === 'THREADS') {
                    board_id = window.location.pathname.split('/')[1]; // We might have moved from /nap/ to /srsbsn/.
                    setTimeout(getposts('section[id], article[id]'), 2000);
                    resolveRefFlags();
                    nsetup.init();
                }

                var addedNode = mutation.addedNodes[0].nodeName;
                if (addedNode === 'HEADER') { // When you post.
                    let data = 'post_nr=' + encodeURIComponent(mutation.target.id) + '&board=' + encodeURIComponent(board_id) + '&regions=' + encodeURIComponent(regions) + '&version=' + encodeURIComponent(version);
                    MakeRequest(
                        'POST',
                        back_end + api_post,
                        data,
                        function () {
                            postNrs.push(mutation.target.id);
                            resolveRefFlags();
                        });
                }

                // Someone else posts. Also checks to see if you're hovering over a quote.
                // This might not work for Doushio.
                if (addedNode === 'ARTICLE' && mutation.target.nodeName !== "BODY" && mutation.target.id !== 'hover_overlay') {
                    postNrs.push(mutation.addedNodes[0].id);
                    setTimeout(resolveRefFlags, 1500);
                }
            }
        });
    }).observe(document.body, { childList: true, subtree: true });
}