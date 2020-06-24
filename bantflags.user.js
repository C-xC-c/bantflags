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
// @version     2.1.0
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

// This will print a load of shit to the console
const debugMode = false;

const isGM4 = typeof GM_setValue === 'undefined';
const setValue = isGM4 ? GM.setValue : GM_setValue;
const getValue = isGM4 ? GM.getValue : GM_getValue;
const xmlHttpRequest = isGM4 ? GM.xmlHttpRequest : GM_xmlhttpRequest;

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

const makeElement = (tag, options) => Object.assign(document.createElement(tag), options);
const toggleFlagButton = state => document.getElementById('append_flag_button').disabled = state === 'off' ? true : false;
const flagSource = flag => flag_dir + flag + '.png';

/** Add styles to the <head> */
const addStyle = css => document.head.appendChild(makeElement('style', { innerHTML: css }));

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
	const selectedFlags = document.querySelectorAll(".bantflags_flag");

	for (let i = 0; i < selectedFlags.length; i++) {
		regions[i] = selectedFlags[i].title;
	}

	setValue(namespace, regions);
}

/** Add a flag to our selection. */
function setFlag(flag) {
	const flagName = flag ? flag : document.querySelector('#flagSelect input').value;
	const flagContainer = document.getElementById('bantflags_container');

	flagContainer.appendChild(makeElement('img', {
		title: flagName,
		src: flagSource(flagName),
		className: 'bantflags_flag',
		onclick: function() {
			flagContainer.removeChild(this);
			if (flagsLoaded)
				toggleFlagButton('on');
			saveFlags();
		}
	}));

	if (flagContainer.children.length >= max_flags)
		toggleFlagButton('off');
	
	if (!flag) // We've added a new flag to our selection
		saveFlags();	
}

function init() {
	const flagsForm = makeElement('div', {
		className: 'flagsForm',
		innerHTML: '<span id="bantflags_container"></span><button type="button" id="append_flag_button" title="Click to add selected flag to your flags. Click on flags to remove them." disabled="true"><<</button><button id="flagLoad" type="button">Click to load flags.</button><div id="flagSelect" ><ul class="hide"></ul><input type="button" value="(You)" onclick=""></div>'
	});

	// Where do we append the flagsForm to?
	if (software.yotsuba) { document.getElementById('delform').appendChild(flagsForm); }
	else if (software.nodegucaDoushio) { document.querySelector('section').insertAdjacentElement('afterEnd', flagsForm); }
	
	for (let i = 0; i < regions.length; i++) {
		setFlag(regions[i]);
	}

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
			let flagInput = flagSelect.querySelector('input');
			let flagList = flagSelect.querySelector('ul');

			let flags = resp.responseText.split('\n');
			for (let i = 0; i < flags.length; i++) {
				const flag = flags[i];
				flagList.appendChild(makeElement('li',{
					innerHTML: `<img src="${flagSource(flag)}" title="${flag}"><span>${flag}</span>`
				}));
			}			

			flagSelect.addEventListener('click', e => {
				// Maybe we clicked the flag image
				const node = e.target.nodeName === 'LI' ? e.target : e.target.parentNode;
				if (node.nodeName === 'LI')
					flagInput.value = node.querySelector('span').innerHTML;
				
				flagList.classList.toggle('hide');
			});

			const flagButton = document.getElementById('append_flag_button');
			flagButton.addEventListener('click', () => setFlag());
			flagButton.disabled = false;
			
			document.getElementById('flagLoad').style.display = 'none';
			document.querySelector('.flagsForm').style.marginRight = "200px"; // flagsForm has position: absolute and is ~200px long.
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
				console.log('[bantflags] Couldn\'t load flags. Refresh the page');
				debug(resp.responseText);
				return;
			}

			const jsonData = JSON.parse(resp.responseText);
			debug(`JSON: ${resp.responseText}`);
			
			Object.keys(jsonData).forEach(post => {
				const flags = jsonData[post];

				if (flags.length <= 0) return;
				
				debug(`Resolving flags for >>${post}`);

				let flagContainer;
				if (software.yotsuba) { flagContainer = document.querySelector(`[id="pc${post}"] .postInfo .nameBlock`); }
				else if (software.foolfuuka) { flagContainer = document.querySelector(`[id="${post}"] .post_data .post_type`); }
				else if (software.nodegucaDoushio) { flagContainer = document.querySelector(`[id="${post}"] header`); }
				
				for (let i = 0; i < flags.length; i++) {
					const flag = flags[i];
					
					flagContainer.append(makeElement('a', {
						innerHTML: `<img src="${flagSource(flag)}" title="${flag}">`,
						className: 'bantFlag',
						target: '_blank',
						title: flag
					}));

					debug(`\t -> ${flag}`);
				}
			});

			postNrs = [];
		});
}

function main() {
	if (!regions) {
		regions = [];
	}
	
	// See Docs/styles.css
	addStyle('.bantFlag{padding: 0px 0px 0px 5px; display: inline-block; width: 16px; height: 11px; position: relative;} .bantflags_flag{padding: 1px;} [title^="Romania"]{ position: relative; animation: shakeAnim 0.1s linear infinite;} @keyframes shakeAnim{ 0% {left: 1px;} 25% {top: 2px;} 50% {left: 1px;} 75% {left: 0px;} 100% {left: 2px;}}.flagsForm{float: right; clear: right; margin: 0 20px 10px 0;} #flagSelect{display:none;}	 #flagSelect ul{list-style-type: none;padding: 0;margin-bottom: 0;cursor: pointer;bottom: 100%;height: 200px;overflow: auto;position: absolute;width:200px;background-color:#fff} #flagSelect ul li {display: block;} #flagSelect ul li:hover {background-color: #ddd;}#flagSelect {position: absolute;}#flagSelect input {width: 200px;} #flagSelect .hide {display: none;}#flagSelect img {margin-left: 2px;}')

	if (software.yotsuba) {
		getPosts('.postContainer');

		addStyle('.flag{top: 0px;left: -1px}');
		init();
	}

	else if (software.nodegucaDoushio) {
		getPosts('section[id], article[id]');

		addStyle('.bantFlag {cursor: default} .bantFlag img {pointer-events: none;}');
		init();
	}

	else if (software.foolfuuka) {
		getPosts('article[id]');

		addStyle('.bantFlag{top: -2px !important;left: -1px !important}');
	}
	
	board_id = window.location.pathname.split('/')[1];
	debug(`board: ${board_id}`);

	try {
		resolveFlags();
	}
	catch (fuckywucky) {
		console.log(`Wah! Manx fucked something up ;~;\nPoke him somewhere with this:\n${fuckywucky}`)
	}
}

if (isGM4) { // Fuck you GreaseMonkey
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
	const e_detail = e => e.detail || e.wrappedJSObject.detail// what?
	document.addEventListener('QRPostSuccessful', e => postFlags(e_detail(e).postID));
	document.addEventListener('4chanQRPostSuccess', e => postFlags(e_detail(e).postId));

	document.addEventListener('ThreadUpdate', e => {
		const d = e_detail(e)
		if (d[404]) return;

		d.newPosts.forEach(post => postNrs.push(post.split('.')[1]));

		resolveFlags();
	});

	document.addEventListener('4chanThreadUpdated', e => {
		const d = e_detail(e);
		if (d.count <= 0) return;
		
		// Get the added posts in reverse order, take post numbers from ID
		const posts = document.querySelectorAll('.postContainer');
		for (let i = 0; i < d.count; i++) {
			postNrs.push(posts[posts.length - 1 - i].id.substr(2));
		}
		
		resolveFlags();
	});
}

if (software.nodegucaDoushio) {
	const postFunc = () => {
		postNrs.push(mutation.target.id);
		resolveFlags();
	}

	const badNodes = ['HR', 'SECTION'];
	
	new MutationObserver(mutations => {
		mutations.forEach(mutation => {
			if (mutation.addedNodes.length <= 0)
				return; // We only care if something post was added
			
			const firstAddedNode = mutation.addedNodes[0].nodeName;

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
