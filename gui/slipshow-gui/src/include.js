import { open } from '@tauri-apps/api/dialog';
// Open a selection dialog for image files
import { readTextFile, BaseDirectory } from '@tauri-apps/api/fs';
// Read the text file in the `$APPCONFIG/app.conf` path
async function f () {
    const selected = await open({
        multiple: false
    });
    const contents = await readTextFile(selected);
    return contents;
    // if (Array.isArray(selected)) {
    //     // user selected multiple files
    // } else if (selected === null) {
    //     // user cancelled the selection
    // } else {
    //     // user selected a single file
    // }
}

window.f = f;
