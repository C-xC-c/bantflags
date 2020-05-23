// ==UserScript==
// @name        /bant/ Flags
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
// @version     1.5.2
// @grant       GM_xmlhttpRequest
// @grant       GM_getValue
// @grant       GM_setValue
// @grant       GM.setValue
// @grant       GM.getValue
// @grant       GM.xmlHttpRequest
// @run-at      document-idle
// @icon        https://flags.plum.moe/flags/0077.png
// @updateURL    https://flags.plum.moe/bantflags.meta.js
// @downloadURL  https://flags.plum.moe/bantflags.user.js
// ==/UserScript==

// (C) Copyright 2019 C-xC-c <boku@plum.moe>
// This file is part of /bant/ Flags.
// /bant/ Flags is licensed under the GNU AGPL Version 3.0 or later.
// see the LICENSE file or <https://www.gnu.org/licenses/>

// Change this if you want verbose debuging information in the console.
const debugMode = true;

const isGM4 = typeof GM_setValue === 'undefined';
const setValue = isGM4 ? GM.setValue : GM_setValue;
const getValue = isGM4 ? GM.getValue : GM_getValue;
const xmlHttpRequest = isGM4 ? GM.xmlHttpRequest : GM_xmlhttpRequest;

//
// DO NOT EDIT ANYTHING IN THIS SCRIPT DIRECTLY - YOUR FLAGS SHOULD BE CONFIGURED USING THE FLAG SELECT
//
const version = encodeURIComponent(2); // Breaking changes.
const back_end = 'https://flags.plum.moe/';
const api_flags = back_end + 'api/flags';
const flag_dir = back_end + 'flags/';
const api_get = back_end + 'api/get';
const api_post = back_end + 'api/post';
const namespace = 'BintFlegs';

// If you increase this the server will ignore your post.
const max_flags = 30;

let regions = []; // The flags we have selected.
let postNrs = []; // all post numbers in the thread.
let board_id = ""; // The board we get flags for.
let flagsLoaded = false;
//
// DO NOT EDIT ANYTHING IN THIS SCRIPT DIRECTLY - YOUR FLAGS SHOULD BE CONFIGURED USING THE FLAG SELECT
//

const debug = text => {
  if (debugMode)
    console.log('[BantFlags] ' + text);
}

// Test unqiue CSS paths to figure out what board software we're using.
const software = {
  yotsuba: window.location.host === 'boards.4chan.org',
  nodegucaDoushio: document.querySelector('b[id="sync"], span[id="sync"]') !== null,
  foolfuuka: document.querySelector('div[id="main"] article header .post_data') !== null
};

const createAndAssign = (element, source) => Object.assign(document.createElement(element), source);

const toggleFlagButton = state => document.getElementById('append_flag_button').disabled = state === 'off' ? true : false;

const flagSource = flag => flag_dir + flag + ".png";

/** Add styles to the <head> */
const addGlobalStyle = css => document.head.appendChild(createAndAssign('style', { innerHTML: css }));

/** Wrapper around GM_xmlhttpRequest.
 * @param {string} method - The HTTP method (GET, POST).
 * @param {string} url - The URL of the request.
 * @param {string} data - text for the form body.
 * @param {Function} func - The function run when we recieve a response. Response data is sent directly to it. */
const makeRequest = ((method, url, data, func) => {
  xmlHttpRequest({
    method: method,
    url: url,
    data: data,
    headers: { "Content-Type": 'application/x-www-form-urlencoded' },
    onload: func
  });
});

/** Itterate over selected flags are store them across browser sessions.*/
function saveFlags() {
  regions = [];
  const selectedFlags = document.querySelectorAll("bantflags_flag");

  for (var i = 0; i < selectedFlags.length; i++) {
    regions[i] = selectedFlags[i].title;
  }

  setValue(namespace, regions);
}

/** Add a flag to our selection.
 * @param {string} flag - The flag to add to our selection. Either passed from saved flags or the current value of flagSelect */
function setFlag(flag) {
  let UID = Math.random().toString(36).substring(7);
  let flagName = flag ? flag : document.querySelector('#flagSelect input').value;
  let flagContainer = document.getElementById('bantflags_container');

  flagContainer.appendChild(createAndAssign('img', {
    title: flagName,
    src: flagSource(flagName),
    id: UID,
    className: 'bantflags_flag'
  }));

  if (flagContainer.children.length >= max_flags)
    toggleFlagButton('off');

  document.getElementById(UID).addEventListener("click", e => {
    flagContainer.removeChild(e.target);
    toggleFlagButton('on');
    saveFlags();
  });

  if (!flag) // We've added a new flag to our selection
    saveFlags();
}

function init() {
  let flagsForm = createAndAssign('div', {
    className: 'flagsForm',
    innerHTML: '<span id="bantflags_container"></span><button type="button" id="append_flag_button" title="Click to add selected flag to your flags. Click on flags to remove them. Saving happens automatically, you only need to refresh the pages that have an outdated flaglist on the page."><<</button><button id="flagLoad" type="button">Click to load flags.</button><div id="flagSelect" ><ul class="hide"></ul><input type="button" value="(You)" onclick=""></div>'
  });

  // Where do we append the flagsForm to?
  if (software.yotsuba) { document.getElementById('delform').appendChild(flagsForm); }
  else if (software.nodegucaDoushio) { document.querySelector('section').append(flagsForm); } // As posts are added the flagForm moves up the page. Could we append this after .section?

  for (let i = 0; i < regions.length; i++) {
    setFlag(regions[i]);
  }

  document.getElementById('append_flag_button').addEventListener('click', () => flagsLoaded ? setFlag() : alert('Load flags before adding them.'));
  document.getElementById('flagLoad').addEventListener('click', makeFlagSelect, { once: true });
}

/** Get flag data from server and fill flags form. */
function makeFlagSelect() {
  makeRequest(
    "GET",
    api_flags,
    "", // We can't send data, it's a GET request.
    function (resp) {
      debug('Loading flags.');
			
      if (resp.status !== 200) {
				console.log('Couldn\'t get flag list from server')
				return;
			}
			
      let flagSelect = document.getElementById('flagSelect');
      let flagList = flagSelect.querySelector('ul');
      let flagInput = flagSelect.querySelector('input');
      let flags = resp.responseText.split('\n');

      for (var i = 0; i < flags.length; i++) {
        let flag = flags[i];
        flagList.appendChild(createAndAssign('li',{
					innerHTML: `<img src="${flagSource(flag)}" title="${flag}"><span>${flag}</span>`
				}));
      }

      flagSelect.addEventListener('click', function (e) {
				// So it works if we click the flag image
				const node = e.target.nodeName === 'LI' ? e.target : e.target.parentNode;
        if (node.nodeName === 'LI') {
					flagInput.value = node.querySelector('span').innerHTML;
				}
				
        flagList.classList.toggle('hide');
      });

      document.getElementById('flagLoad').style.display = 'none';
      document.querySelector('.flagsForm').style.marginRight = "200px"; // Element has position: absolute and is ~200px long.
      flagSelect.style.display = 'inline-block';
      flagsLoaded = true;
    });
}

/** add all of the post numbers on the page to postNrs. */
function getPosts(selector) {
  const posts = document.querySelectorAll(selector);

  for (let i = 0; i < posts.length; i++) {
    const postNumber = software.yotsuba
					? posts[i].id.substr(2) // Fuck you 4chan.
					: posts[i].id;

		postNrs.push(postNumber);
	}
  debug(postNrs);
}

/** Get flags from the database using values in postNrs and pass the response on to onFlagsLoad */
function resolveFlags() {
  makeRequest(
    'POST',
    api_get,
    'post_nrs=' + encodeURIComponent(postNrs) + '&board=' + encodeURIComponent(board_id) + '&version=' + version,
    function (resp) {

      if (resp.status !== 200) {
        console.log('[bantflags] Couldn\'t load flags. Refresh the page.');
        return;
      }
			
			const jsonData = JSON.parse(resp.responseText);
			debug(`JSON: ${resp.responseText}`);
			
			Object.keys(jsonData).forEach(post => {
				let flags = jsonData[post];

				if (flags.length <= 0)
					return;
				
				debug(`Resolving flags for >>${post}`);

				let flagContainer;
				if (software.yotsuba) { flagContainer = document.querySelector(`[id="pc${post}"] .postInfo .nameBlock`); }
				else if (software.foolfuuka) { flagContainer = document.querySelector(`[id="${post}"] .post_data .post_type`); }
				else if (software.nodegucaDoushio) { flagContainer = document.querySelector(`[id="${post}"] header`); }
				
				for (let i = 0; i < flags.length; i++) {
					const flag = flags[i];

					const newFlag = createAndAssign('a', {
						innerHTML: `<img src="${flagSource(flag)}" title="${flag}">`,
						className: 'bantFlag',
						target: '_blank',
						title: flag
					});
					
					flagContainer.append(newFlag);

					debug(`\t -> ${flag}`);
				}
			});

			postNrs = [];
    });
}

function main() {
  if (!regions) { // Should only be called before you set flags for the first time.
    regions = [];
    window.confirm('[BantFlags]: No Flags detected.\nIf this is your first time running bantflags, look for the "Click to load flags." button at the bottom right of the thread, then select your flag and press the ">>" button.');
  }

  // See Docs/styles.css
	addGlobalStyle('.bantFlag{padding: 0px 0px 0px 5px; display: inline-block; width: 16px; height: 11px; position: relative;} .bantflags_flag{padding: 1px;} [title^="Romania"]{ position: relative; animation: shakeAnim 0.1s linear infinite;} @keyframes shakeAnim{ 0% {left: 1px;} 25% {top: 2px;} 50% {left: 1px;} 75% {left: 0px;} 100% {left: 2px;}}.flagsForm{float: right; clear: right; margin: 20px 10px;} #flagSelect{display:none;}  #flagSelect ul{list-style-type: none;padding: 0;margin-bottom: 0;cursor: pointer;bottom: 100%;height: 200px;overflow: auto;position: absolute;width:200px;background-color:#fff} #flagSelect ul li {display: block;} #flagSelect ul li:hover {background-color: #ddd;}#flagSelect {position: absolute;}#flagSelect input {width: 200px;} #flagSelect .hide {display: none;}#flagSelect img {margin-left: 2px;}')

  if (software.yotsuba) {
    getPosts('.postContainer');

    addGlobalStyle('.flag{top: 0px;left: -1px}');
    init();
  }

  if (software.nodegucaDoushio) {
    getPosts('section[id], article[id]');

    addGlobalStyle('.bantFlag {cursor: default} .bantFlag img {pointer-events: none;}');
    init();
  }

  if (software.foolfuuka) {
    getPosts('article[id]');

    addGlobalStyle('.bantFlag{top: -2px !important;left: -1px !important}');
  }
	
  board_id = window.location.pathname.split('/')[1];
	debug(board_id);

	resolveFlags();
}

if (isGM4) { // Fuck you GM4
  (async () => {
    regions = await getValue(namespace);
    main();
  })();
}
else {
  regions = getValue(namespace);
  main();
}

const postFlags = (post_nr, func = resp => debug(resp.responseText)) => makeRequest(
  'POST',
  api_post,
	`post_nr=${encodeURIComponent(post_nr)}&board=${encodeURIComponent(board_id)}&regions=${encodeURIComponent(regions)}&version=${version}`,
	func);

if (software.yotsuba) {
  const GetEvDetail = e => e.detail || e.wrappedJSObject.detail;
	
  // 4chanX and native extension respectively
  document.addEventListener('QRPostSuccessful', e => postFlags(e.detail.postID));
  document.addEventListener('4chanQRPostSuccess', e => postFlags(GetEvDetail(e).postId));

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

    resolveFlags();
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

    resolveFlags();
	}, false);
}

if (software.nodegucaDoushio) {

	const postFunc = function() {
		postNrs.push(mutation.target.id);
		resolveFlags();
	}

	const badNodes = ['HR', 'SECTION'];
	
  new MutationObserver(mutations => {
    mutations.forEach(mutation => {
			if (mutation.addedNodes.length <= 0)
				return; // We only care if something post was added
			
      var firstAddedNode = mutation.addedNodes[0].nodeName;

			// Enter a thread / change boards
      if (mutation.target.nodeName === 'THREADS') {
				if (badNodes.includes(firstAddedNode))
					return; // We are in the index and a post was added, handled properly further down
				
        board_id = window.location.pathname.split('/')[1];
        setTimeout(getPosts('section[id], article[id]'), 2000);
        resolveFlags();
        init();
      }

      // We post
      if (firstAddedNode === 'HEADER') {
				postFlags(mutation.target.id, postFunc)
			}

			// Someone else posts
      if (firstAddedNode === 'ARTICLE') {
				if (mutation.target.nodeName === 'BODY' || mutation.target.id === 'hover_overlay')
					return; // User is hovering over a post
				
        postNrs.push(mutation.addedNodes[0].id);
        setTimeout(resolveFlags, 1500);
      }
		});
	}).observe(document.body, { childList: true, subtree: true });
}
