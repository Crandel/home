#!/usr/bin/env python3

# This script keeps track of active keyboard layouts per window.
#
# This script requires i3ipc-python package (install it from a system package
# manager or pip).
import asyncio
from i3ipc.aio import Connection
from i3ipc import Event


class Layout:
    def __init__(self):
        self.windows = {}
        self.prev_focused = -1

    async def window_await(self, ipc, event):
        if event.change == "focus":
            await self.on_window_focus(ipc, event)
        elif event.change == "close":
            await self.on_window_close(ipc, event)

    async def on_window_focus(self, ipc, event):
        # Save current layout
        layouts = {
            input.identifier: input.xkb_active_layout_index
            for input in await ipc.get_inputs()
        }
        self.windows[self.prev_focused] = layouts
        window_id = event.container.id
        # Restore layout of the newly focused window
        if window_id in self.windows:
            for (input_id, layout_index) in self.windows[window_id].items():
                if layout_index != layouts[input_id]:
                    await ipc.command(f'input "{input_id}" xkb_switch_layout {layout_index}')

        self.prev_focused = window_id

    async def on_window_close(self, ipc, event):
        windows_id = event.container.id
        if windows_id in self.windows:
            del(self.windows[windows_id])


async def main():
    ipc = await Connection(auto_reconnect=True).connect()
    layout = Layout()
    ipc.on(Event.WINDOW, layout.window_await)
    await ipc.main()


asyncio.get_event_loop().run_until_complete(main())
