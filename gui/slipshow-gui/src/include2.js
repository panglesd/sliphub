(()=>{var e,t;/**
 * Transforms a callback function to a string identifier that can be passed to the backend.
 * The backend uses the identifier to `eval()` the callback.
 *
 * @return A unique identifier associated with the callback function.
 *
 * @since 1.0.0
 */function o(e,t=!1){let o=window.crypto.getRandomValues(new Uint32Array(1))[0],a=`_${o}`;return Object.defineProperty(window,a,{value:o=>(t&&Reflect.deleteProperty(window,a),e?.(o)),writable:!1,configurable:!0}),o}/**
 * Sends a message to the backend.
 * @example
 * ```typescript
 * import { invoke } from '@tauri-apps/api/tauri';
 * await invoke('login', { user: 'tauri', password: 'poiwe3h4r5ip3yrhtew9ty' });
 * ```
 *
 * @param cmd The command name.
 * @param args The optional arguments to pass to the command.
 * @return A promise resolving or rejecting to the backend response.
 *
 * @since 1.0.0
 */async function a(e,t={}){return new Promise((a,n)=>{let i=o(e=>{a(e),Reflect.deleteProperty(window,`_${c}`)},!0),c=o(e=>{n(e),Reflect.deleteProperty(window,`_${i}`)},!0);window.__TAURI_IPC__({cmd:e,callback:i,error:c,...t})})}// Copyright 2019-2023 Tauri Programme within The Commons Conservancy
// SPDX-License-Identifier: Apache-2.0
// SPDX-License-Identifier: MIT
/** @ignore */async function n(e){return a("tauri",e)}// Copyright 2019-2023 Tauri Programme within The Commons Conservancy
// SPDX-License-Identifier: Apache-2.0
// SPDX-License-Identifier: MIT
/**
 * Native system dialogs for opening and saving files.
 *
 * This package is also accessible with `window.__TAURI__.dialog` when [`build.withGlobalTauri`](https://tauri.app/v1/api/config/#buildconfig.withglobaltauri) in `tauri.conf.json` is set to `true`.
 *
 * The APIs must be added to [`tauri.allowlist.dialog`](https://tauri.app/v1/api/config/#allowlistconfig.dialog) in `tauri.conf.json`:
 * ```json
 * {
 *   "tauri": {
 *     "allowlist": {
 *       "dialog": {
 *         "all": true, // enable all dialog APIs
 *         "ask": true, // enable dialog ask API
 *         "confirm": true, // enable dialog confirm API
 *         "message": true, // enable dialog message API
 *         "open": true, // enable file open API
 *         "save": true // enable file save API
 *       }
 *     }
 *   }
 * }
 * ```
 * It is recommended to allowlist only the APIs you use for optimal bundle size and security.
 * @module
 *//**
 * Open a file/directory selection dialog.
 *
 * The selected paths are added to the filesystem and asset protocol allowlist scopes.
 * When security is more important than the easy of use of this API,
 * prefer writing a dedicated command instead.
 *
 * Note that the allowlist scope change is not persisted, so the values are cleared when the application is restarted.
 * You can save it to the filesystem using [tauri-plugin-persisted-scope](https://github.com/tauri-apps/plugins-workspace/tree/v1/plugins/persisted-scope).
 * @example
 * ```typescript
 * import { open } from '@tauri-apps/api/dialog';
 * // Open a selection dialog for image files
 * const selected = await open({
 *   multiple: true,
 *   filters: [{
 *     name: 'Image',
 *     extensions: ['png', 'jpeg']
 *   }]
 * });
 * if (Array.isArray(selected)) {
 *   // user selected multiple files
 * } else if (selected === null) {
 *   // user cancelled the selection
 * } else {
 *   // user selected a single file
 * }
 * ```
 *
 * @example
 * ```typescript
 * import { open } from '@tauri-apps/api/dialog';
 * import { appDir } from '@tauri-apps/api/path';
 * // Open a selection dialog for directories
 * const selected = await open({
 *   directory: true,
 *   multiple: true,
 *   defaultPath: await appDir(),
 * });
 * if (Array.isArray(selected)) {
 *   // user selected multiple directories
 * } else if (selected === null) {
 *   // user cancelled the selection
 * } else {
 *   // user selected a single directory
 * }
 * ```
 *
 * @returns A promise resolving to the selected path(s)
 *
 * @since 1.0.0
 */async function i(e={}){return"object"==typeof e&&Object.freeze(e),n({__tauriModule:"Dialog",message:{cmd:"openDialog",options:e}})}/**
 * Reads a file as an UTF-8 encoded string.
 * @example
 * ```typescript
 * import { readTextFile, BaseDirectory } from '@tauri-apps/api/fs';
 * // Read the text file in the `$APPCONFIG/app.conf` path
 * const contents = await readTextFile('app.conf', { dir: BaseDirectory.AppConfig });
 * ```
 *
 * @since 1.0.0
 */async function c(e,t={}){return n({__tauriModule:"Fs",message:{cmd:"readTextFile",path:e,options:t}})}// Read the text file in the `$APPCONFIG/app.conf` path
async function p(){let e=await i({multiple:!1}),t=await c(e);return t;// if (Array.isArray(selected)) {
//     // user selected multiple files
// } else if (selected === null) {
//     // user cancelled the selection
// } else {
//     // user selected a single file
// }
}(t=e||(e={}))[t.Audio=1]="Audio",t[t.Cache=2]="Cache",t[t.Config=3]="Config",t[t.Data=4]="Data",t[t.LocalData=5]="LocalData",t[t.Desktop=6]="Desktop",t[t.Document=7]="Document",t[t.Download=8]="Download",t[t.Executable=9]="Executable",t[t.Font=10]="Font",t[t.Home=11]="Home",t[t.Picture=12]="Picture",t[t.Public=13]="Public",t[t.Runtime=14]="Runtime",t[t.Template=15]="Template",t[t.Video=16]="Video",t[t.Resource=17]="Resource",t[t.App=18]="App",t[t.Log=19]="Log",t[t.Temp=20]="Temp",t[t.AppConfig=21]="AppConfig",t[t.AppData=22]="AppData",t[t.AppLocalData=23]="AppLocalData",t[t.AppCache=24]="AppCache",t[t.AppLog=25]="AppLog",window.f=p})();//# sourceMappingURL=include.js.map

//# sourceMappingURL=include.js.map
