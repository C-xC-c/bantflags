// ==UserScript==
// @name        /bant/ Flags for GreaseMonkey 4
// @namespace   BintFlegs_gm4
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
// @version     1.4.0
// @grant       GM.xmlHttpRequest
// @grant       GM.getValue
// @grant       GM.setValue
// @run-at      document-idle
// @icon        https://flags.plum.moe/flags/0077.png
// @updateURL    https://flags.plum.moe/bantflags.gm4.meta.js
// @downloadURL  https://flags.plum.moe/bantflags.gm4.user.js
// ==/UserScript==

// (C) Copyright 2019 C-xC-c <boku@plum.moe>
// This file is part of /bant/ Flags.
// /bant/ Flags is licensed under the GNU AGPL Version 3.0 or later.
// see the LICENSE file or <https://www.gnu.org/licenses/>

// Change this if you want verbose debuging information in the console.
const debugMode = true;

// For idiots using GM4. Does anyone else not support GM_ functions?
if (typeof GM === 'undefined') {
  if (window.confirm('[BantFlags] This version of bantflags is specifically for GreaseMonkey 4.0 +. Press \'ok\' to download correct version.')) {
    window.location.href = "https://flags.plum.moe/bantflags.user.js"; // Should I use thhis or window.location.replace?
  }
  return;
}

//
// DO NOT EDIT ANYTHING IN THIS SCRIPT DIRECTLY - YOUR FLAGS SHOULD BE CONFIGURED USING THE FLAG SELECT
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
//
// DO NOT EDIT ANYTHING IN THIS SCRIPT DIRECTLY - YOUR FLAGS SHOULD BE CONFIGURED USING THE FLAG SELECT
//

// Test unqiue CSS paths to figure out what board software we're using.
const software = {
  yotsuba: window.location.host === 'boards.4chan.org',
  nodegucaDoushio: document.querySelector('b[id="sync"], span[id="sync"]') !== null,
  foolfuuka: document.querySelector('div[id="main"] article header .post_data') !== null
};

// This is kinda necessary but I don't know how much. Can I just wrap checking regions on startup or is the whole script required? I won't be testing that for now.
(async () => {

  /** Wrapper around Object.assign and document.createElement
   * @param {string} element - The HTML Element to create.
   * @param {Object} source - The properties to assign to the element. 
   * @returns {object} The HTML tag created*/
  const createAndAssign = (element, source) => Object.assign(document.createElement(element), source);

  /** Add a stylesheet to the head of the document.
   * @param {string} css - The CSS rules for the stylesheet. 
   * @returns {object} The style element appended to the head */
  const addGlobalStyle = css => document.head.appendChild(createAndAssign('style', {
    type: 'text/css',
    innerHTML: css
  }));

  /** Write extra information to the console if debugMode is set to true.
   * @param {string} text - The text to write to the console. */
  function debug(text) {
    if (debugMode) {
      console.log('[BantFlags] ' + text);
    }
  }

  /** Wrapper around GM.xmlhttpRequest.
   * @param {string} method - The HTTP method (GET, POST).
   * @param {string} url - The URL of the request.
   * @param {string} data - Data sent inn the form body.
   * @param {Function} func - The function run when we recieve a response. Response data is sent directly to it. */
  const MakeRequest = ((method, url, data, func) => {
    GM.xmlHttpRequest({
      method: method,
      url: url,
      data: data,
      headers: { "Content-Type": 'application/x-www-form-urlencoded' },
      onload: func
    });
  });

  /** Try some MakeRequest again if it fails.
  * @param {function} func - The function to retry.
  * @param {XMLHttpRequest} resp  - The XMLHttpResponse from the failed request.*/
  function retry(func, resp) {
    console.log('[BantFlags] Could not fetch flags, status: ' + resp.status);
    console.log(resp.statusText);
    setTimeout(func, requestRetryInterval);
  }

  // TODO: this shouldn't be a object.
  var nsetup = { // not anymore a clone of the original setup
    namespace: 'BintFlegs', // TODO: should be const.
    flagsLoaded: false,
    form: '<span id="bantflags_container"></span><button type="button" id="append_flag_button" title="Click to add selected flag to your flags. Click on flags to remove them. Saving happens automatically, you only need to refresh the pages that have an outdated flaglist on the page."><<</button><button id="flagLoad" type="button">Click to load flags.</button><div id="flagSelect" ><ul class="hide"></ul><input type="button" value="(You)" onclick=""></div>',
    fillHtml: function () { // TODO: this function should have a better name. Only called by nsetup.init, can be inlined?
      MakeRequest(
        "GET",
        back_end + api_flags,
        "version=" + encodeURIComponent(version),
        function (resp) {
          debug('Loading flags.');
          if (resp.status !== 200) {
            retry(nsetup.fillHtml, resp);
            return;
          }

          let flagSelect = document.getElementById('flagSelect');
          let flagList = flagSelect.querySelector('ul');
          let flagInput = flagSelect.querySelector('input');
          let flags = resp.responseText.split('\n');

          for (var i = 0; i < flags.length; i++) {
            let flag = flags[i];
            flagList.appendChild(createAndAssign('li', {
              innerHTML: '<img src="' + back_end + flag_dir + flag + '.png" title="' + flag + '"> <span>' + flag + '</span>'
            }));
          }

          flagSelect.addEventListener('click', (e) => {
            listItem = e.target.nodeName === 'LI' ? e.target : e.target.parentNode;
            if (listItem.nodeName === 'LI') {
              flagInput.value = listItem.querySelector('span').innerHTML;
            }
            flagList.classList.toggle('hide');
          });

          document.getElementById('flagLoad').style.display = 'none';
          document.querySelector('.flagsForm').style.marginRight = "200px";
          flagSelect.style.display = 'inline-block';
          nsetup.flagsLoaded = true;
        });
    },
    save: function () {
      let storedFlags = [];
      let selectedFlags = document.getElementsByClassName("bantflags_flag");

      for (var i = 0; i < selectedFlags.length; i++) {
        storedFlags[i] = selectedFlags[i].title;
      }

      GM.setValue(nsetup.namespace, storedFlags);
      regions = GM.getValue(nsetup.namespace);
    },
    setFlag: function (flag) {
      let UID = Math.random().toString(36).substring(7);
      let flagName = flag ? flag : document.querySelector('#flagSelect input').value;
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

      document.getElementById(UID).addEventListener("click", (e) => {
        flagContainer.removeChild(e.target);
        nsetup.toggleFlagButton('on');
        nsetup.save();
      });

      if (!flag) { // When we add a flag to our selection, save it for when we reload the page.
        nsetup.save();
      }
    },
    init: function () {
      let flagsForm = createAndAssign('div', {
        className: 'flagsForm',
        innerHTML: nsetup.form
      });

      // Where do we append the flagsForm to?
      if (software.yotsuba) { document.getElementById('delform').appendChild(flagsForm); }
      if (software.nodegucaDoushio) { document.querySelector('section').append(flagsForm); } // As posts are added the flagForm moves up the page. Could we append this after .section?

      for (var i in regions) {
        nsetup.setFlag(regions[i]);
      }

      document.getElementById('append_flag_button').addEventListener('click',
        () => nsetup.flagsLoaded ? nsetup.setFlag() : alert('Load flags before adding them.'));

      document.getElementById('flagLoad').addEventListener('click', nsetup.fillHtml, { once: true });
    },
    toggleFlagButton: state => document.getElementById('append_flag_button').disabled = state === 'off' ? true : false
  };

  /** Select all of the post numbers on the page.
   * @param {string} selector - The CSS selector of the post numbers. */
  function getPosts(selector) {
    let posts = document.querySelectorAll(selector);

    for (var i = 0; i < posts.length; i++) {
      let postNumber = software.yotsuba
        ? posts[i].id.replace('pc', '') // Fuck you 4chan.
        : posts[i].id;
      postNrs.push(postNumber);
    }
    debug(postNrs);
  }

  /** Take the response from resolveRefFlags and append flags to their respective post numbers.
   * @param {XMLHttpRequest} response - The response data from resolveRefFlags. */
  function onFlagsLoad(response) {
    debug('JSON: ' + response.responseText);
    var jsonData = JSON.parse(response.responseText);

    Object.keys(jsonData).forEach(function (post) {

      // Get the post header with a CSS selector. Different for each board software.
      var flagContainer;
      if (software.nodegucaDoushio) { flagContainer = document.querySelector('[id="' + post + '"] header'); }
      if (software.yotsuba) { flagContainer = document.querySelector('[id="pc' + post + '"] .postInfo  .nameBlock'); }
      if (software.foolfuuka) { flagContainer = document.querySelector('[id="' + post + '"] .post_data .post_type'); }

      let flags = jsonData[post];
      if (flags.length > 0) {
        console.log('[BantFlags] Resolving flags for >>' + post);

        for (var i = 0; i < flags.length; i++) {
          let flag = flags[i];

          let newFlag = createAndAssign('a', {
            innerHTML: '<img src="' + back_end + flag_dir + flag + '.png" title="' + flag + '"> ',
            className: 'bantFlag',
            target: '_blank'
          });

          if (software.foolfuuka) {
            newFlag.style = 'padding: 0px 0px 0px ' + (3 + 2 * (i > 0)) + 'px; vertical-align:;display: inline-block; width: 16px; height: 11px; position: relative;';
          }

          if (software.nodegucaDoushio) {
            newFlag.title = flag;
          }

          flagContainer.append(newFlag);

          console.log('\t -> ' + flag);
        }
      }
    });

    postNrs = [];
  }

  /** Get flags from the database using values in postNrs and pass the response on to onFlagsLoad */
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

  // This one await is what stops regular bantflags from working with GM4 lole.
  regions = await GM.getValue(nsetup.namespace);
  if (!regions) { // No flags set.
    regions = [];
    window.confirm('[BantFlags]: No Flags detected.');
  }

  // See Docs/styles.css
  addGlobalStyle('.flagsForm{float: right; clear: right; margin: 20px 10px;} #flagSelect{display:none;} .bantflags_flag{padding: 1px;} [title^="Romania"]{ position: relative; animation: shakeAnim 0.1s linear infinite;} @keyframes shakeAnim{ 0% {left: 1px;} 25% {top: 2px;} 50% {left: 1px;} 75% {left: 0px;} 100% {left: 2px;}} #flagSelect ul {list-style-type: none;padding: 0;margin-bottom: 0;cursor: pointer;bottom: 100%;height: 200px;overflow: auto;position: absolute;width:200px;background-color:#fff}#flagSelect ul li {display: block;}#flagSelect ul li:hover {background-color: #ddd;}#flagSelect {position: absolute;}#flagSelect input {width: 200px;} #flagSelect .hide {display: none;}#flagSelect img {margin-left: 2px;}');

  // Flags need to be parsed differently and have different styles depending on board software.
  if (software.yotsuba) {
    debug('4chan');
    board_id = 'bant';
    getPosts('.postContainer');

    addGlobalStyle('.bantFlag {padding: 0px 0px 0px 5px; vertical-align:;display: inline-block; width: 16px; height: 11px; position: relative;} .flag{top: 0px !important;left: -1px !important}');
  }

  if (software.nodegucaDoushio) {
    debug('Nineball');
    board_id = window.location.pathname.split('/')[1]; // 'nap' or 'srsbsn'
    getPosts('section[id], article[id]');

    addGlobalStyle('.bantFlag {cursor: default} .bantFlag img {pointer-events: none;}');
  }

  if (software.foolfuuka) {
    debug('FoolFuuka');
    board_id = 'bant';
    getPosts('article[id]');

    addGlobalStyle('.bantFlag{top: -2px !important;left: -1px !important}');
  }

  resolveRefFlags();

  if (software.yotsuba) {
    const GetEvDetail = e => e.detail || e.wrappedJSObject.detail;

    const postFlags = post_nr => MakeRequest(
      'POST',
      back_end + api_post,
      'post_nr=' + encodeURIComponent(post_nr) + '&board=' + encodeURIComponent(board_id) + '&regions=' + encodeURIComponent(regions) + '&version=' + encodeURIComponent(version),
      func = resp => debug(resp.responseText));

    // Send flags to the backend when we makle a post. Top is 4chanX, bottom is native extension.
    document.addEventListener('QRPostSuccessful', e => postFlags(e.detail.postID), false);
    document.addEventListener('4chanQRPostSuccess', e => postFlags(GetEvDetail(e).postId), false);

    // I need to look at these.
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

    document.addEventListener('4chanThreadUpdated', function (e) {
      var evDetail = GetEvDetail(e);
      let threadID = window.location.pathname.split('/')[3];
      let postsContainer = Array.prototype.slice.call(document.getElementById('t' + threadID).childNodes);
      let lastPosts = postsContainer.slice(Math.max(postsContainer.length - evDetail.count, 1)); //get the last n elements (where n is evDetail.count)

      lastPosts.forEach(function (post_container) {
        var post_nr = post_container.id.replace('pc', '');
        postNrs.push(post_nr);
      });

      resolveRefFlags();
    }, false);

    nsetup.init();
  }

  if (software.nodegucaDoushio) {
    nsetup.init();

    // This is poking at the mutations made on the page to figure out what happened and thus what actions to take.
    // There is full support for nodeguca but I don't have a Doushio board I feel comfortable spamming to ensure it works properly there. There is at least partial support.
    new MutationObserver(function (mutations) {
      mutations.forEach(function (mutation) {
        if (mutation.addedNodes.length > 0) { // A post was added.
          var firstAddedNode = mutation.addedNodes[0].nodeName;

          // Enter a thread or change boards. Checks for when a post is added while in the index.
          if (mutation.target.nodeName === 'THREADS' && firstAddedNode !== 'HR' && firstAddedNode !== 'SECTION') {
            board_id = window.location.pathname.split('/')[1];
            setTimeout(getPosts('section[id], article[id]'), 2000);
            resolveRefFlags();
            nsetup.init();
          }

          // You post.
          if (firstAddedNode === 'HEADER') {
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

          // Someone else posts. Checks to see if you're hovering over a post.
          if (firstAddedNode === 'ARTICLE' && mutation.target.nodeName !== "BODY" && mutation.target.id !== 'hover_overlay') {
            postNrs.push(mutation.addedNodes[0].id);
            setTimeout(resolveRefFlags, 1500);
          }
        }
      });
    }).observe(document.body, { childList: true, subtree: true });
  }
})();