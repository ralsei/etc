# pylint: disable=C0111
import os, operator
from qutebrowser.api import interceptor, message

config = config # noqa: F821 pylint: disable=E0602,C0103,W0127
c = c # noqa: F821 pylint: disable=E0602,C0103,W0127

### ricing
config.source("/etc/nixos/config/qutebrowser/colors.py")

c.fonts.default_family = "IBM Plex Mono"
c.tabs.position = 'top'
c.tabs.show = 'multiple'
c.tabs.title.format = '{current_title}'
c.tabs.title.alignment = 'center'
c.downloads.position = 'bottom'
c.tabs.favicons.show = 'never'
c.tabs.indicator.width = 0

def make_padding(top, bottom, left, right):
    return {
        'top': top,
        'bottom': bottom,
        'left': left,
        'right': right
    }

c.tabs.padding = make_padding(3, 3, 0, 0)
c.tabs.indicator.padding = make_padding(0, 0, 0, 0)

### behavior
if 'EDITOR' in os.environ:
    c.editor.command = [os.environ['EDITOR'] + ' "{}"']

REDIRECT_MAP = {
    "reddit.com": operator.methodcaller('setHost', 'old.reddit.com'),
    "www.reddit.com": operator.methodcaller('setHost', 'old.reddit.com'),
}

def redirect_intercept(info):
    """Block the given request if necessary."""
    if (info.resource_type != interceptor.ResourceType.main_frame
            or info.request_url.scheme() in {"data", "blob"}):
        return
    url = info.request_url
    redir = REDIRECT_MAP.get(url.host())
    if redir is not None and redir(url) is not False:
        message.info("Redirecting to " + url.toString())
        info.redirect(url)

interceptor.register(redirect_intercept)

### bindings
def nmap(key, command):
    """Bind key to command in normal mode."""
    config.bind(key, command, mode='normal')

nmap(',m', 'spawn mpv {url}')
nmap(',M', 'hint links spawn mpv {hint-url}')
