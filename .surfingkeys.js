settings.showModeStatus = true;
settings.defaultSearchEngine = "d";

// Custom keymaps
unmap('H');
unmap('S');
mapkey('H', '#4Go back in history', function() {
	history.go(-1);
}, {repeatIgnore: true});
mapkey('S', '#8Open opened URL in current tab', function() {
	Front.openOmnibar({type: "URLs", extra: "getTabURLs"});
});

map('F', 'af');
unmap('gf');
map('gf', 'f');
unmap('f');

// Domain specific settings
// if ( document.domain === "habr.com" || document.domain === "www.youtube.com" ) {
// }

// Ace editor settings
// settings.aceKeybindings = "emacs";
aceVimMap('xx', ':wq', 'insert');

// Ukrainian keyboard
map('а', 'f');
map('А', 'F');
map('б', ',');
map('Б', '<');
map('в', 'd');
map('В', 'D');
map('г', 'u');
map('Г', 'U');
map('д', 'l');
map('Д', 'L');
map('е', 't');
map('Е', 'T');
map('ж', ';');
map('Ж', ':');
map('з', 'p');
map('З', 'P');
map('и', 'b');
map('И', 'B');
map('і', 's');
map('І', 'S');
map('ї', ']');
map('Ї', '}');
map('й', 'q');
map('Й', 'Q');
map('к', 'r');
map('К', 'R');
map('л', 'k');
map('Л', 'K');
map('м', 'v');
map('М', 'V');
map('н', 'y');
map('Н', 'Y');
map('о', 'j');
map('О', 'J');
map('п', 'g');
map('П', 'G');
map('р', 'h');
map('Р', 'H');
map('с', 'c');
map('С', 'C');
map('т', 'n');
map('Т', 'N');
map('у', 'e');
map('У', 'E');
map('ф', 'a');
map('Ф', 'A');
map('х', '[');
map('Х', '{');
map('ц', 'w');
map('Ц', 'W');
map('ч', 'x');
map('Ч', 'X');
map('ш', 'i');
map('Ш', 'I');
map('щ', 'o');
map('Щ', 'O');
map('ь', 'm');
map('Ь', 'M');
map('ю', '.');
map('Ю', '>');
map('я', 'z');
map('Я', 'Z');
map('па', 'gf');

aceVimMap('ш', 'i', 'normal');
aceVimMap('чч', ':wq', 'insert');

map('<Ctrl-i>', '<Ctrl-ш>');

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
