unmap('H');
unmap('S');
mapkey('H', '#4Go back in history', function() {
	history.go(-1);
}, {repeatIgnore: true});
mapkey('S', '#8Open opened URL in current tab', function() {
	Front.openOmnibar({type: "URLs", extra: "getTabURLs"});
});

map('F', 'af');

// Domain specific settings
if ( document.domain === "habr.com" || document.domain === "www.youtube.com" ) {
	unmap('gf');
	map('gf', 'f');
	unmap('f');
}

// set theme
settings.theme = `
:root {
	--theme-ace-bg:#282828ab; /*Note the fourth channel, this adds transparency*/
	--theme-ace-bg-accent:#3c3836;
	--theme-ace-fg:#ebdbb2;
	--theme-ace-fg-accent:#7c6f64;
	--theme-ace-cursor:#928374;
	--theme-ace-select:#458588;
	--sk-theme-annotation:#a31db1
}

.sk_theme {
	background: #100a14dd;
	color: #D4DA1B;
}
.sk_theme tbody {
	color: #292d;
}
.sk_theme input {
	color: #d9dce0;
}
.sk_theme .url {
	color: #2d9501;
}
.sk_theme .annotation {
	color: var(--sk-theme-annotation);
}
.sk_theme .omnibar_highlight {
	color: #333;
	background: #C981FCaa;
}
.sk_theme #sk_omnibarSearchResult ul>li:nth-child(odd) {
	background: #5d4d7a55;
}
.sk_theme #sk_omnibarSearchResult ul>li.focused {
	background: #5d4d7aaa;
}
.sk_theme #sk_omnibarSearchResult .omnibar_folder {
	color: var(--sk-theme-annotation);
}

#sk_editor {
	height: 50% !important; /*Remove this to restore the default editor size*/
	background: var(--theme-ace-bg) !important;
}
.ace-chrome .ace_print-margin, .ace_gutter, .ace_gutter-cell, .ace_dialog{
	background: var(--theme-ace-bg-accent) !important;
}
.ace_dialog-bottom{
	border-top: 1px solid var(--theme-ace-bg) !important;
}
.ace-chrome{
	color: var(--theme-ace-fg) !important;
}
.ace_gutter, .ace_dialog {
	color: var(--theme-ace-fg-accent) !important;
}
.ace_cursor{
	color: var(--theme-ace-cursor) !important;
}
.normal-mode .ace_cursor{
	background-color: var(--theme-ace-cursor) !important;
	border: var(--theme-ace-cursor) !important;
}
.ace_marker-layer .ace_selection {
	background: var(--theme-ace-select) !important;
}
`;

// settings.aceKeybindings = "emacs";
settings.showModeStatus = true;
settings.defaultSearchEngine = "d";
