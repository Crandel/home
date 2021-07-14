// 1. Start remote debugging from desktop Firefox. (about:debugging)
// 2. After you connect to android firefox app, you go to about:config page.
// 3. Inject a script to a web page that opened from Android.
// 4. Execute whole script to android firefox.
var user_pref = function(pref, val){

  try{
    if(typeof val == "string"){

         Services.prefs.setStringPref(pref, val);
    }
    else if(typeof val == "number"){

         Services.prefs.setIntPref(pref, val);
    }
    else if(typeof val == "boolean"){

         Services.prefs.setBoolPref(pref, val);
    }
  } catch(e){
    console.log("pref:" + pref + " val:" + val + " e:" + e);
  }
}
//user_pref("layout.css.devPixelsPerPx", 1.25);
user_pref("accessibility.force_disabled", true);
user_pref("accessibility.typeaheadfind", false); // Search for text when start typing
user_pref("app.normandy.api_url", "");
user_pref("app.normandy.enabled", false);
user_pref("app.shield.optoutstudies.enabled", false);
user_pref("app.update.auto", false);
user_pref("app.update.checkInstallTime", false);
user_pref("apz.allow_double_tap_zooming", true);
user_pref("apz.allow_zooming", true);
user_pref("apz.android.chrome_fling_physics.enabled", true);
user_pref("apz.frame_delay.enabled", false);
user_pref("apz.gtk.kinetic_scroll.enabled", false);
user_pref("beacon.enabled", false); // PREF: Disable "beacon" asynchronous HTTP transfers (used for analytics)
user_pref("browser.aboutConfig.showWarning", false); // for the new HTML version [FF71+]
// user_pref("browser.bookmarks.openInTabClosesMenu", false); // Prevent bookmark menu and toolbar folder menu from closing when opening bookmark in a new tab
// user_pref("browser.bookmarks.restore_default_bookmarks", false);
// user_pref("browser.bookmarks.showMobileBookmarks", true);
// user_pref("browser.cache.disk.parent_directory", "/tmp/firefox");
// user_pref("browser.contentblocking.customBlockList.preferences.ui.enabled", true);
// user_pref("browser.ctrlTab.previews", true);
user_pref("browser.discovery.enabled", false); // "Allow Firefox to make personalized extension recommendations"
user_pref("browser.display.background_color", "#2e2e31");
user_pref("browser.display.foreground_color", "#f9f9fa");
//user_pref("browser.display.use_document_fonts", 0);
//user_pref("browser.download.autohideButton", false);
//user_pref("browser.download.panel.shown", true);
user_pref("browser.engagement.total_uri_count.pbm", false); // Turn off counting URIs in private browsing mode
user_pref("browser.fixup.domainwhitelist.gateway.localhost", true);
user_pref("browser.fixup.domainwhitelist.gateway.test", true);
user_pref("browser.fixup.hide_user_pass", true); // PREF: When browser.fixup.alternate.enabled is enabled, strip password from 'user:password@...' URLs
user_pref("browser.helperApps.deleteTempFileOnExit", true); // remove temp files opened with an external application
// user_pref("browser.history_swipe_animation.disabled", false); // For macOS only
user_pref("browser.in-content.dark-mode", true); // Dark mode in content pages
// user_pref("browser.library.activity-stream.enabled", true); // For macOS only
// user_pref("browser.link.open_newwindow", 3); // open links targeting new windows in a new tab instead
// user_pref("browser.link.open_newwindow.restriction", 0);
// user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons", false); // PREF: Disable Extension recommendations (Firefox >= 65)
// user_pref("browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features", false); // PREF: Disable Extension recommendations (Firefox >= 65)
// user_pref("browser.newtabpage.activity-stream.feeds.places", true);
// user_pref("browser.newtabpage.activity-stream.feeds.section.highlights", false);
// user_pref("browser.newtabpage.activity-stream.feeds.section.topstories", false); // PREF: Disable "Recommended by Pocket" in Firefox Quantum
// user_pref("browser.newtabpage.activity-stream.feeds.telemetry", false);
// user_pref("browser.newtabpage.activity-stream.filterAdult", false);
// user_pref("browser.newtabpage.activity-stream.prerender", true);
// user_pref("browser.newtabpage.activity-stream.section.highlights.includePocket", false);
// user_pref("browser.newtabpage.activity-stream.showSearch", false);
// user_pref("browser.newtabpage.activity-stream.showSponsored", false);
// user_pref("browser.newtabpage.activity-stream.telemetry", false);
// user_pref("browser.newtabpage.activity-stream.telemetry.ping.endpoint", "");
// user_pref("browser.newtabpage.activity-stream.tippyTop.service.endpoint", "");
// user_pref("browser.newtabpage.activity-stream.topSitesRows", 3);
// user_pref("browser.newtabpage.enhanced", true);
user_pref("browser.ping-centre.telemetry", false);
user_pref("browser.pocket.enabled", false); // PREF: Disable Pocket
user_pref("browser.privatebrowsing.forceMediaMemoryCache", true); // [FF75+] disable media cache from writing to disk in Private Browsing
user_pref("browser.safebrowsing.allowOverride", false);
user_pref("browser.safebrowsing.blockedURIs.enabled", false);
user_pref("browser.safebrowsing.downloads.enabled", false);
user_pref("browser.safebrowsing.downloads.remote.block_dangerous", false);
user_pref("browser.safebrowsing.downloads.remote.block_dangerous_host", false);
user_pref("browser.safebrowsing.downloads.remote.block_potentially_unwanted", false);
user_pref("browser.safebrowsing.downloads.remote.block_uncommon", false);
user_pref("browser.safebrowsing.downloads.remote.enabled", false);
user_pref("browser.safebrowsing.downloads.remote.url", "");
user_pref("browser.safebrowsing.enabled", false);
user_pref("browser.safebrowsing.malware.enabled", false);
user_pref("browser.safebrowsing.phishing.enabled", false);
user_pref("browser.safebrowsing.provider.google.advisoryURL", "");
user_pref("browser.safebrowsing.provider.google.gethashURL", "");
user_pref("browser.safebrowsing.provider.google.lists", "");
user_pref("browser.safebrowsing.provider.google.pver", "");
user_pref("browser.safebrowsing.provider.google.reportMalwareMistakeURL", "");
user_pref("browser.safebrowsing.provider.google.reportPhishMistakeURL", "");
user_pref("browser.safebrowsing.provider.google.reportURL", "");
user_pref("browser.safebrowsing.provider.google.updateURL", "");
user_pref("browser.safebrowsing.provider.google4.advisoryName", "");
user_pref("browser.safebrowsing.provider.google4.advisoryURL", "");
user_pref("browser.safebrowsing.provider.google4.dataSharingURL", "");
user_pref("browser.safebrowsing.provider.google4.gethashURL", "");
user_pref("browser.safebrowsing.provider.google4.lastupdatetime", "");
user_pref("browser.safebrowsing.provider.google4.lists", "");
user_pref("browser.safebrowsing.provider.google4.nextupdatetime", "");
user_pref("browser.safebrowsing.provider.google4.pver", "");
user_pref("browser.safebrowsing.provider.google4.reportMalwareMistakeURL", "");
user_pref("browser.safebrowsing.provider.google4.reportPhishMistakeURL", "");
user_pref("browser.safebrowsing.provider.google4.reportURL", "");
user_pref("browser.safebrowsing.provider.google4.updateURL", "");
user_pref("browser.safebrowsing.provider.mozilla.gethashURL", "");
user_pref("browser.safebrowsing.provider.mozilla.lists", "");
user_pref("browser.safebrowsing.provider.mozilla.pver", "");
user_pref("browser.safebrowsing.provider.mozilla.updateURL", "");
user_pref("browser.safebrowsing.reportPhishURL", "");
user_pref("browser.search.countryCode", "US"); // PREF: Disable GeoIP lookup on your address to set default search engine region
user_pref("browser.search.geoSpecificDefaults", false); // PREF: Don't use Mozilla-provided location-specific search engines
user_pref("browser.search.geoSpecificDefaults.url", "");
user_pref("browser.search.geoip.url", "");
user_pref("browser.search.hiddenOneOffs", "Bing,Amazon.com,Twitter");
user_pref("browser.search.openintab", true); // opens search results in new tab
user_pref("browser.search.region", "US");
user_pref("browser.search.separatePrivateDefault", true);
user_pref("browser.search.separatePrivateDefault.ui.enabled", true);
user_pref("browser.search.suggest.enabled", false);
user_pref("browser.search.suggest.enabled.private", false);
user_pref("browser.search.update", false);
user_pref("browser.search.widget.inNavBar", true); // Add search widget to navbar
user_pref("browser.send_pings", false); // PREF: Enable pinging URIs specified in HTML <a> ping= attributes
user_pref("browser.send_pings.require_same_host", true); // PREF: When browser pings are enabled, only allow pinging the same host as the origin page
user_pref("browser.sessionhistory.max_total_viewers", 18); // affects how many pages Firefox stores in such a way that they load super fast
user_pref("browser.sessionstore.interval", 1800000);
user_pref("browser.shell.checkDefaultBrowser", false);
user_pref("browser.slowStartup.notificationDisabled", true);
user_pref("browser.startup.blankWindow", false);
// user_pref("browser.startup.page", 3);
// user_pref("browser.tabs.closeWindowWithLastTab", false); // The last tab does not close the browser
// user_pref("browser.tabs.drawInTitlebar", true);
user_pref("browser.tabs.loadDivertedInBackground", true); // Open browser immediately after open link
user_pref("browser.tabs.loadInBackground", false);
// user_pref("browser.tabs.remote.autostart", true);
// user_pref("browser.tabs.tabMinWidth", 30);
// user_pref("browser.tabs.warnOnClose", false);
user_pref("browser.touchmode.auto", true);
user_pref("browser.uidensity", 1); // Use small density in toolbar
user_pref("browser.uitour.enabled", false);
user_pref("browser.uitour.url", "");
user_pref("browser.urlbar.autoFill", false);
user_pref("browser.urlbar.autocomplete.enabled", false);
user_pref("browser.urlbar.clickSelectsAll", true);
user_pref("browser.urlbar.decodeURLsOnCopy", true);
user_pref("browser.urlbar.maxRichResults", 15);
user_pref("browser.urlbar.megabar", true); // Enable megabar
user_pref("browser.urlbar.richSuggestions.tail", false);
user_pref("browser.urlbar.speculativeConnect.enabled", false); // PREF: Disable preloading of autocomplete URLs
user_pref("browser.urlbar.suggest.bookmark", true);
user_pref("browser.urlbar.suggest.history", true);
user_pref("browser.urlbar.suggest.searches", false);
user_pref("browser.urlbar.trimURLs", false); // PREF: Don't trim HTTP off of URLs in the address bar.
user_pref("browser.zoom.siteSpecific", false);
user_pref("camera.control.face_detection.enabled", false); // PREF: Disable face detection
user_pref("captivedetect.canonicalURL", "");
user_pref("datareporting.healthreport.uploadEnabled", false);
user_pref("datareporting.policy.dataSubmissionEnabled", true);
user_pref("datareporting.policy.firstRunURL", "");
user_pref("device.sensors.enabled", false); // PREF: Disable sensor API
user_pref("device.sensors.motion.enabled", false);
user_pref("device.sensors.orientation.enabled", false);
// user_pref("devtools.aboutdebugging.showSystemAddons", true);
// user_pref("devtools.netmonitor.features.webSockets", true);
// user_pref("devtools.onboarding.telemetry.logged", true);
// user_pref("devtools.theme", "dark");
// user_pref("devtools.toolbox.splitconsoleEnabled", false);
// user_pref("devtools.webide.enabled", false); // [DEFAULT: false FF70+]
user_pref("dom.enable_resource_timing", false); // PREF: Disable resource timing API
// user_pref("dom.event.clipboardevents.enabled", true);
user_pref("dom.flyweb.enabled", false); // PREF: Disable FlyWeb (discovery of LAN/proximity IoT devices that expose a Web interface)
user_pref("dom.gamepad.enabled", false); // PREF: Disable gamepad API to prevent USB device enumeration
user_pref("dom.gamepad.extensions.enabled", false);
user_pref("dom.image-lazy-loading.enabled", true);
user_pref("dom.ipc.processCount", 3);
user_pref("dom.maxHardwareConcurrency", 3); // PREF: Spoof dual-core CPU
user_pref("dom.max_script_run_time", 30); // PREF: script execution time
user_pref("dom.netinfo.enabled", false); // PREF: Disable leaking network/browser connection information via Javascript
user_pref("dom.push.enabled", false);
user_pref("dom.security.https_only_mode", false);  // Force to open https
user_pref("dom.security.https_only_mode.upgrade_local", false);  // ignore localhost
user_pref("dom.vibrator.enabled", true); // PREF: Disable vibrator API
user_pref("dom.vr.enabled", false); // PREF: Disable virtual reality devices APIs
user_pref("dom.webgpu.enabled", true);
user_pref("dom.webnotifications.requireuserinteraction", true);
user_pref("editor.truncate_user_pastes", false); // prevents that the server receives a longer than expected password or string.
user_pref("experiments.activeExperiment", false); // PREF: Disable Mozilla experiments
user_pref("experiments.enabled", false);
user_pref("experiments.manifest.uri", "");
user_pref("experiments.supported", false);
user_pref("extensions.fxmonitor.enabled", false);
user_pref("extensions.getAddons.cache.enabled", false);
user_pref("extensions.getAddons.showPane", false);
user_pref("extensions.htmlaboutaddons.discover.enabled", false);
user_pref("extensions.htmlaboutaddons.recommendations.enabled", false);
user_pref("extensions.pocket.enabled", false);
user_pref("extensions.ui.dictionary.hidden", false);
user_pref("extensions.ui.experiment.hidden", false);
user_pref("extensions.ui.locale.hidden", false);
user_pref("extensions.webextensions.remote", true);
user_pref("extensions.webextensions.restrictedDomains", "accounts-static.cdn.mozilla.net,accounts.firefox.com,oauth.accounts.firefox.com,profile.accounts.firefox.com,sync.services.mozilla.com"); // blacklist for webextensions
user_pref("extensions.webextensions.userScripts.enabled", true);
user_pref("findbar.highlightAll", true);
user_pref("font.internaluseonly.changed", true);
//user_pref("font.minimum-size.x-cyrillic", 10);
//user_pref("font.minimum-size.x-western", 10);
//user_pref("font.name-list.emoji", "Noto Color Emoji, Twemoji Mozilla");
//user_pref("font.name.monospace.x-cyrillic", "Hack Nerd Font Mono");
//user_pref("font.name.monospace.x-western", "Hack Nerd Font Mono");
//user_pref("font.name.sans-serif.x-cyrillic", "Fira Sans");
//user_pref("font.name.sans-serif.x-western", "Fira Sans");
//user_pref("font.name.serif.x-cyrillic", "Liberation Serif");
//user_pref("font.name.serif.x-western", "Liberation Serif");
//user_pref("full-screen-api.ignore-widgets", true);
user_pref("full-screen-api.transition-duration.enter", "0 0");
user_pref("full-screen-api.transition-duration.leave", "0 0");
user_pref("full-screen-api.warning.timeout", 0);
user_pref("general.smoothScroll.currentVelocityWeighting", "0");
user_pref("general.smoothScroll.durationToIntervalRatio", 1000);
user_pref("general.smoothScroll.lines.durationMaxMS", 150);
user_pref("general.smoothScroll.lines.durationMinMS", 0);
user_pref("general.smoothScroll.mouseWheel.durationMaxMS", 150);
user_pref("general.smoothScroll.mouseWheel.durationMinMS", 0);
user_pref("general.smoothScroll.mouseWheel.migrationPercent", 0);
user_pref("general.smoothScroll.msdPhysics.continuousMotionMaxDeltaMS", 250);
user_pref("general.smoothScroll.msdPhysics.enabled", true);
user_pref("general.smoothScroll.msdPhysics.motionBeginSpringConstant", 450);
user_pref("general.smoothScroll.msdPhysics.regularSpringConstant", 450);
user_pref("general.smoothScroll.msdPhysics.slowdownMinDeltaMS", 50);
user_pref("general.smoothScroll.msdPhysics.slowdownMinDeltaRatio;0", 4);
user_pref("general.smoothScroll.msdPhysics.slowdownSpringConstant", 5000);
user_pref("general.smoothScroll.other", true);
user_pref("general.smoothScroll.other.durationMaxMS", 150);
user_pref("general.smoothScroll.other.durationMinMS", 0);
user_pref("general.smoothScroll.pages.durationMaxMS", 150);
user_pref("general.smoothScroll.pages.durationMinMS", 0);
user_pref("general.smoothScroll.pixels", true);
user_pref("general.smoothScroll.pixels.durationMaxMS", 150);
user_pref("general.smoothScroll.pixels.durationMinMS", 0);
user_pref("general.smoothScroll.scrollbars.durationMaxMS", 600);
user_pref("general.smoothScroll.scrollbars.durationMinMS", 0);
user_pref("general.smoothScroll.stopDecelerationWeighting", "0.2");
user_pref("general.warnOnAboutConfig", false); // XUL/XHTML version
user_pref("geo.wifi.logging.enabled", false); // PREF: When geolocation is enabled, don't log geolocation requests to the console
user_pref("geo.wifi.uri", "");
user_pref("gestures.enable_single_finger_input", true);
user_pref("gfx.font_rendering.cleartype_params.rendering_mode", 5);
user_pref("gfx.use_text_smoothing_setting", true);
user_pref("gfx.webrender.all", true);
user_pref("gfx.webrender.compositor", true);
user_pref("gfx.webrender.enabled", true);
user_pref("gfx.webrender.highlight-painted-layers", false);
user_pref("gfx.webrender.software", false);
user_pref("gfx.work-around-driver-bugs", false);
user_pref("image.avif.enabled", true);
user_pref("intl.accept_languages", "en-us,en,ru,uk,de"); // PREF: Set Accept-Language HTTP header to en-US regardless of Firefox localization
user_pref("intl.locale.matchOS", false); // PREF: Don't use OS values to determine locale, force using Firefox locale setting
user_pref("intl.locale.requested", "en-US");
user_pref("javascript.options.shared_memory", true);
user_pref("javascript.options.wasm", true); // PREF: Enable WebAssembly
user_pref("javascript.use_us_english_locale", true); // [HIDDEN PREF]
user_pref("keyword.enabled", true); // PREF: Submit invalid URIs entered in the address bar to the default search engine
user_pref("layers.acceleration.force-enabled", false);
user_pref("layers.amd-switchable-gfx.enabled", false); // AMD specific settings
user_pref("layers.async-pan-zoom.enabled", true);
user_pref("layers.geometry.d3d11.enabled", false); // Windows specific settings
user_pref("layers.gpu-process.enabled", false);
user_pref("layout.css.osx-font-smoothing.enabled", true);
user_pref("layout.frame_rate", 90);
user_pref("layout.spellcheckDefault", 2); // enable spell-check for all text boxes
user_pref("layout.word_select.stop_at_punctuation", true);
// user_pref("lightweightThemes.selectedThemeID", "firefox-compact-dark@mozilla.org");
user_pref("media.autoplay.allow-muted", false);
user_pref("media.autoplay.block-event.enabled", true);
user_pref("media.autoplay.default", 5);
user_pref("media.autoplay.enabled", false);
user_pref("media.autoplay.enabled.user-gestures-needed", false);
user_pref("media.av1.enabled", true);
user_pref("media.devices.insecure.enabled", true);
user_pref("media.eme.enabled", false); // disable all DRM content (EME: Encryption Media Extension)
user_pref("media.encoder.webm.enabled", true);
user_pref("media.ffmpeg.vaapi.enabled", false);
user_pref("media.ffvpx.enabled", false);
user_pref("media.getdisplaymedia.enabled", true);
user_pref("media.gmp-widevinecdm.enabled", false); // Disables playback of DRM-controlled HTML5 content
user_pref("media.gpu-process-decoder", true);
user_pref("media.navigator.enabled", true); // websites can track the microphone and camera status of your device.
user_pref("media.navigator.mediadatadecoder_vpx_enabled", true);
user_pref("media.peerconnection.ice.no_host", false); // PREF: reveal your internal IP when WebRTC is enabled
user_pref("media.rdd-process.enabled", true);
user_pref("media.rdd-vpx.enabled", false);
//user_pref("media.videocontrols.picture-in-picture.enabled", true);
//user_pref("media.videocontrols.picture-in-picture.video-toggle.always-show", true);
//user_pref("media.videocontrols.picture-in-picture.video-toggle.enabled", true);
//user_pref("media.videocontrols.picture-in-picture.video-toggle.flyout-enabled", true);
user_pref("media.webspeech.recognition.enable", false); // PREF: Disable speech recognition
user_pref("media.webspeech.synth.enabled", false); // PREF: Disable speech synthesis
// user_pref("middlemouse.paste", true);
// user_pref("mousewheel.acceleration.factor", 10);
// user_pref("mousewheel.acceleration.start", -1);
// user_pref("mousewheel.default.delta_multiplier_x", 100);
// user_pref("mousewheel.default.delta_multiplier_y", 100);
// user_pref("mousewheel.default.delta_multiplier_z", 100);
// user_pref("mousewheel.min_line_scroll_amount", 36);
// user_pref("mousewheel.system_scroll_override_on_root_content.enabled", false);
// user_pref("mousewheel.transaction.timeout", 2000);
user_pref("network.IDN_show_punycode", true); // Not rendering IDNs as their Punycode equivalent leaves you open to phishing attacks
user_pref("network.allow-experiments", false);
user_pref("network.captive-portal-service.enabled", false); // check if network is public wifi
user_pref("network.cookie.prefsMigrated", true);
user_pref("network.cookie.thirdparty.sessionOnly", true);
user_pref("network.dns.disableIPv6", false);
user_pref("network.dns.disablePrefetch", true);
user_pref("network.dns.disablePrefetchFromHTTPS", true);
user_pref("network.dns.echconfig.enabled", true);
user_pref("network.dns.skipTRR-when-parental-control-enabled", false);
user_pref("network.dns.use_https_rr_as_altsvc", true);
user_pref("network.http.altsvc.enabled", false); // https://www.usenix.org/conference/woot19/presentation/tiwari
user_pref("network.http.altsvc.oe", false);
user_pref("network.http.http3.enabled", true);
user_pref("network.http.redirection-limit", 10);
user_pref("network.http.referer.XOriginPolicy", 0);
user_pref("network.http.referer.defaultPolicy", 1);
user_pref("network.http.spdy.enabled.http2", true);
user_pref("network.http.speculative-parallel-limit", 0);
// user_pref("network.manage-offline-status", false);
user_pref("network.predictor.enable-prefetch", false);
user_pref("network.predictor.enabled", false);
user_pref("network.prefetch-next", false); // After the browser is finished loading the page, it begins silently prefetching specified documents and stores them in its cache
user_pref("network.proxy.socks_remote_dns", false);
user_pref("network.stricttransportsecurity.preloadlist", true);
user_pref("network.tcp.tcp_fastopen_enable", true);
user_pref("network.trr.bootstrapAddress", "176.103.130.130");
user_pref("network.trr.custom_uri", "https://dns.adguard.com/dns-query");
user_pref("network.trr.enable_when_nrpt_detected", true);
user_pref("network.trr.enable_when_proxy_detected", true);
user_pref("network.trr.enable_when_vpn_detected", true);
user_pref("network.trr.mode", 3);
user_pref("network.trr.uri", "https://dns.adguard.com/dns-query");
user_pref("network.trr.wait-for-A-and-AAAA", false);
user_pref("network.warnOnAboutNetworking", false);
user_pref("nglayout.initialpaint.delay", 0);
user_pref("pdfjs.disabled", true);
user_pref("permissions.default.desktop-notification", 2);
user_pref("permissions.default.geo", 2);
user_pref("privacy.donottrackheader.enabled", true);
user_pref("privacy.firstparty.isolate", false); // for jira
user_pref("privacy.resistFingerprinting", false);
user_pref("privacy.resistFingerprinting.autoDeclineNoUserInputCanvasPrompts", false); // Add dialog for canvas API
user_pref("privacy.resistFingerprinting.block_mozAddonManager", true); // enable webextensions on mozilla websites
user_pref("privacy.resistFingerprinting.letterboxing", false);
user_pref("privacy.resistFingerprinting.target_video_res", 1080);
user_pref("privacy.trackingprotection.cryptomining.enabled", true);
user_pref("privacy.trackingprotection.enabled", true);
user_pref("privacy.trackingprotection.fingerprinting.enabled", true);
user_pref("privacy.trackingprotection.pbmode.enabled", true);
user_pref("privacy.trackingprotection.socialtracking.enabled", true);
user_pref("privacy.userContext.enabled", true); // Multicontainer enables
user_pref("privacy.userContext.longPressBehavior", 2);
user_pref("privacy.userContext.ui.enabled", true);
// user_pref("privacy.webrtc.hideGlobalIndicator", true);
// user_pref("privacy.webrtc.legacyGlobalIndicator", false);
user_pref("reader.color_scheme", "dark");
user_pref("reader.content_width", 12);
user_pref("reader.font_type", "sans-serif");
user_pref("reader.parse-on-load.enabled", true);
user_pref("reader.parse-on-load.force-enabled", true);
user_pref("reader.toolbar.vertical", true);
user_pref("security.dialog_enable_delay", 1000); // PREF: Ensure you have a security delay when installing add-ons (milliseconds)
user_pref("security.mixed_content.block_active_content", false);
user_pref("security.mixed_content.upgrade_display_content", true);
user_pref("security.secure_connection_icon_color_gray", false); // Return green lock for https (Firefox >= 70)
user_pref("security.ssl.errorReporting.automatic", false);
user_pref("security.ssl.errorReporting.enabled", false);
user_pref("security.ssl.errorReporting.url", "");
user_pref("security.tls.version.enable-deprecated", false);
// user_pref("services.sync.addons.ignoreUserEnabledChanges", true); // exclude sync of addons status enabled/disabled
// user_pref("services.sync.prefs.sync.browser.newtabpage.activity-stream.section.highlights.includePocket", false);
// user_pref("services.sync.prefs.sync.browser.newtabpage.enabled", false); // exclude sync newtabpage icons
user_pref("signon.generation.available", false);
user_pref("signon.generation.enabled", false);
user_pref("signon.management.page.mobileAndroidURL", "");
user_pref("svg.context-properties.content.enabled", true); // apply theme color to the toolbar button
user_pref("toolkit.cosmeticAnimations.enabled", false);
user_pref("toolkit.identity.enabled", false);
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true); // Turn on userChrome.css and userContent.css support
// user_pref("toolkit.scrollbox.horizontalScrollDistance", 6);
// user_pref("toolkit.scrollbox.verticalScrollDistance", 6);
user_pref("toolkit.telemetry.archive.enabled", true);
user_pref("toolkit.telemetry.bhrPing.enabled", true);
user_pref("toolkit.telemetry.coverage.opt-out", true);
user_pref("toolkit.telemetry.enabled", true);
user_pref("toolkit.telemetry.firstShutdownPing.enabled", true);
user_pref("toolkit.telemetry.hybridContent.enabled", true);
user_pref("toolkit.telemetry.newProfilePing.enabled", true);
user_pref("toolkit.telemetry.reportingpolicy.firstRun", true);
user_pref("toolkit.telemetry.shutdownPingSender.enabled", true);
user_pref("toolkit.telemetry.unified", true);
user_pref("toolkit.telemetry.updatePing.enabled", true);
user_pref("ui.prefersReducedMotion", 1); // Disable animation
user_pref("ui.systemUsesDarkTheme", 1);
// user_pref("view_source.editor.external", true); // enable view source using external text editor
// user_pref("view_source.editor.path", "$HOME/.local/bin/editor-run");
user_pref("webextensions.storage.sync.kinto", false);
user_pref("webgl.enable-debug-renderer-info", false);
user_pref("webgl.force-enabled", false);
user_pref("webgl.msaa-force", false);
// user_pref("widget.chrome.allow-gtk-dark-theme", true);
// user_pref("widget.content.allow-gtk-dark-theme", true);
// user_pref("widget.wayland-dmabuf-vaapi.enabled", true);
// user_pref("widget.wayland-dmabuf-video-textures.enabled", true);
// user_pref("widget.wayland-smooth-rendering", true);
// user_pref("widget.wayland_vsync.enabled", false);
