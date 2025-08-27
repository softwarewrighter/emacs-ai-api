Test immediately:
bashclaude
> "Take screenshot of localhost:18080"
This should fix it instantly. The MCP server expects chromium-1179 but drones only has chromium-1187. Hive works because it has the symlink bridging this version mismatch.
The complete 4-command setup should be:

sudo pacman -S chromium
sudo npm install -g playwright @executeautomation/playwright-mcp-server
npx playwright install chromium
ln -sf ~/.cache/ms-playwright/chromium-*/chrome-linux ~/.cache/ms-playwright/chromium-1179 (create version symlink)
claude mcp add playwright -s user -- playwright-mcp-server

The symlink handles version mismatches between what the MCP server expects vs what gets downloaded.