#!/usr/bin/env python3
"""
Supacode session switcher, as a kitty kitten.

Mirrors Supacode's own hierarchy: sessions are grouped under the project they
belong to (repo + branch, the label Supacode puts in its vertical strip), and
each row is one terminal tab with the title, agent and activity Supacode shows.

Data comes from `supacode data`, which joins zmx, Supacode's layouts.json and
sidebar.json, and git into tab-separated rows:
    name mark base age dir repo branch title agent activity bucket

Everything past `age` is optional: without Supacode's state files the rows
still list, just grouped by directory instead of project.

Bound to a hotkey by install.sh (ctrl+shift+g by default).
"""
import os
import subprocess
from typing import Any, Iterator, NamedTuple

from kittens.tui.handler import Handler, result_handler
from kittens.tui.loop import EventType, Loop, MouseButton, MouseEvent
from kittens.tui.operations import MouseTracking, styled

# The path below is substituted by install.sh. The fallback keeps the kitten
# working if it was copied into place by hand.
SUPACODE = '/home/larionov/.local/bin/supacode'
if SUPACODE.startswith('@'):
    import shutil
    SUPACODE = shutil.which('supacode') or os.path.expanduser('~/.local/bin/supacode')


def _config(key: str, default: str) -> str:
    """Ask the CLI for resolved configuration, so there is one source of truth."""
    try:
        out = subprocess.run([SUPACODE, 'config'], capture_output=True,
                             text=True, timeout=15).stdout
    except Exception:
        return default
    for line in out.splitlines():
        k, _, v = line.partition(' ')
        if k == key:
            return v.strip() or default
    return default


HOST = os.environ.get('SUPACODE_HOST') or _config('host', 'remote')
HEADER_ROWS = 2      # title line + query line
FOOTER_ROWS = 1
AGENT_COLORS = {'claude': 'magenta', 'omp': 'cyan', 'codex': 'green'}


class Session(NamedTuple):
    name: str          # supa-<uuid>
    attached: bool
    base: str          # working directory basename
    age: str
    path: str
    repo: str
    branch: str
    label: str         # Supacode's tab title
    agent: str
    activity: str
    bucket: str
    tab_title: str     # what we name the kitty tab

    @property
    def project(self) -> str:
        if self.repo and self.branch:
            return f'{self.repo} · {self.branch}'
        return self.repo or self.branch or self.base

    @property
    def haystack(self) -> str:
        return f'{self.repo} {self.branch} {self.label} {self.base}'


def is_placeholder(label: str) -> bool:
    """Supacode falls back to a path when a tab was never named."""
    return not label or label.startswith(('…', '~', '/'))


def fetch() -> list[Session]:
    out = subprocess.run(
        [SUPACODE, 'data'], capture_output=True, text=True, timeout=60
    ).stdout
    seen: dict[str, int] = {}
    sessions = []
    for line in out.splitlines():
        f = line.split('\t')
        if len(f) < 11:
            continue
        name, mark, base, age, path, repo, branch, label, agent, activity, bucket = f[:11]
        # a short, stable name for the kitty tab
        stem = base if is_placeholder(label) else label
        for junk in ('✳ ', 'π > ', 'π: '):
            stem = stem.replace(junk, '')
        stem = stem.strip() or base
        n = seen[stem] = seen.get(stem, 0) + 1
        sessions.append(Session(
            name=name, attached=mark.strip() == '*', base=base, age=age, path=path,
            repo=repo, branch=branch, label=label, agent=agent, activity=activity,
            bucket=bucket, tab_title=stem if n == 1 else f'{stem}:{n}',
        ))
    return sessions


def fuzzy(query: str, text: str) -> tuple[int, tuple[int, ...]] | None:
    """Subsequence match. Returns (score, matched indices) or None."""
    if not query:
        return 0, ()
    tl, pos, idx, score, prev = text.lower(), 0, [], 0, -2
    for ch in query.lower():
        p = tl.find(ch, pos)
        if p < 0:
            return None
        if p == prev + 1:
            score += 8                                  # contiguous run
        if p == 0 or tl[p - 1] in '-_/. ·':
            score += 5                                  # start of a word
        idx.append(p)
        prev, pos = p, p + 1
    return score - idx[0] // 4 - len(text) // 25, tuple(idx)


class Switcher(Handler):

    mouse_tracking = MouseTracking.buttons_only

    def __init__(self, sessions: list[Session]) -> None:
        super().__init__()
        self.sessions = sessions
        self.query = ''
        self.rows: list[tuple[str, Any]] = []   # ('h', text) | ('i', Session)
        self.sel = 0
        self.top = 0
        self.chosen: Session | None = None
        self.refilter()

    # ---------------------------------------------------------------- state

    def refilter(self) -> None:
        scored: list[tuple[int, Session]] = []
        for s in self.sessions:
            hit = fuzzy(self.query, s.haystack)
            if hit is not None:
                scored.append((hit[0], s))
        if self.query:
            scored.sort(key=lambda t: -t[0])

        # group by project, preserving the order projects first appear
        groups: dict[str, list[Session]] = {}
        for _score, s in scored:
            groups.setdefault(s.project, []).append(s)

        self.rows = []
        for project, items in groups.items():
            self.rows.append(('h', project))
            for s in items:
                self.rows.append(('i', s))
        self.sel = self.first_item()
        self.top = 0

    def first_item(self) -> int:
        for i, (kind, _) in enumerate(self.rows):
            if kind == 'i':
                return i
        return 0

    @property
    def viewport(self) -> int:
        return max(1, self.screen_size.rows - HEADER_ROWS - FOOTER_ROWS)

    def move(self, delta: int) -> None:
        if not self.rows:
            return
        step = 1 if delta > 0 else -1
        remaining = abs(delta)
        i = self.sel
        while remaining:
            j = i + step
            while 0 <= j < len(self.rows) and self.rows[j][0] != 'i':
                j += step               # skip group headers
            if not (0 <= j < len(self.rows)):
                break
            i = j
            remaining -= 1
        self.sel = i
        # keep the selected row, and its header where possible, on screen
        if self.sel < self.top:
            self.top = max(0, self.sel - 1)
        elif self.sel >= self.top + self.viewport:
            self.top = self.sel - self.viewport + 1
        self.draw_screen()

    # --------------------------------------------------------------- render

    def header_line(self, project: str) -> str:
        width = self.screen_size.cols
        text = f' {project}'[:width]
        return styled(text + ' ' * max(0, width - len(text)),
                      fg='blue', bold=True)

    def item_line(self, s: Session, selected: bool) -> str:
        width = self.screen_size.cols
        agent_w = 7
        # cap it so agent/age don't get marooned on a wide window
        label_w = max(16, min(72, width - 4 - agent_w - 5))

        label = s.label if not is_placeholder(s.label) else s.base
        if len(label) > label_w:
            label = label[:label_w - 1] + '…'
        pad = ' ' * (label_w - len(label))

        dot = styled('●', fg='green') if s.attached else styled('◦', dim=True)
        if s.agent:
            agent = styled(f'{s.agent:<{agent_w}}',
                           fg=AGENT_COLORS.get(s.agent, 'yellow'))
        else:
            agent = ' ' * agent_w

        body = (f'   {dot} '
                f'{label if selected else styled(label, dim=is_placeholder(s.label))}'
                f'{pad} {agent} {styled(f"{s.age:>4}", dim=True)}')
        printed = 3 + 1 + 1 + label_w + 1 + agent_w + 1 + 4
        body += ' ' * max(0, width - printed)
        return styled(body, reverse=True) if selected else body

    def draw_screen(self) -> None:
        self.cmd.clear_screen()
        shown = sum(1 for k, _ in self.rows if k == 'i')
        head = styled(' supacode ', fg='black', bg='blue', bold=True)
        head += styled(f'  {shown}/{len(self.sessions)} tabs on {HOST}', dim=True)
        self.print(head)
        self.print(styled('› ', fg='blue', bold=True) + self.query +
                   styled('▏', fg='blue'))

        if not self.rows:
            self.print(styled('  no match', dim=True))
        for i in range(self.top, min(len(self.rows), self.top + self.viewport)):
            kind, payload = self.rows[i]
            if kind == 'h':
                self.print(self.header_line(payload))
            else:
                self.print(self.item_line(payload, i == self.sel))

        hint = ' ↑↓ move   ⏎ switch   esc cancel   click to pick '
        self.cmd.set_cursor_position(0, self.screen_size.rows - 1)
        self.print(styled(hint, dim=True), end='')

    def initialize(self) -> None:
        self.cmd.set_cursor_visible(False)
        self.cmd.set_window_title('supacode switcher')
        self.draw_screen()

    def finalize(self) -> None:
        self.cmd.set_cursor_visible(True)

    def on_resize(self, screen_size: Any) -> None:
        super().on_resize(screen_size)
        self.draw_screen()

    # ----------------------------------------------------------------- input

    def choose(self) -> None:
        if self.rows and self.rows[self.sel][0] == 'i':
            self.chosen = self.rows[self.sel][1]
        self.quit_loop(0)

    def on_text(self, text: str, in_bracketed_paste: bool = False) -> None:
        self.query += text
        self.refilter()
        self.draw_screen()

    def on_key(self, key_event: Any) -> None:
        if key_event.matches('esc') or key_event.matches('ctrl+c'):
            self.quit_loop(1)
        elif key_event.matches('enter'):
            self.choose()
        elif key_event.matches('down') or key_event.matches('ctrl+n'):
            self.move(1)
        elif key_event.matches('up') or key_event.matches('ctrl+p'):
            self.move(-1)
        elif key_event.matches('page_down'):
            self.move(self.viewport // 2)
        elif key_event.matches('page_up'):
            self.move(-self.viewport // 2)
        elif key_event.matches('home'):
            self.move(-len(self.rows))
        elif key_event.matches('end'):
            self.move(len(self.rows))
        elif key_event.matches('backspace'):
            self.query = self.query[:-1]
            self.refilter()
            self.draw_screen()
        elif key_event.matches('ctrl+u'):
            self.query = ''
            self.refilter()
            self.draw_screen()

    def on_mouse_event(self, mouse_event: MouseEvent) -> None:
        if mouse_event.type is not EventType.PRESS:
            return
        if mouse_event.buttons & MouseButton.WHEEL_UP:
            self.top = max(0, self.top - 1)
            self.draw_screen()
        elif mouse_event.buttons & MouseButton.WHEEL_DOWN:
            self.top = min(max(0, len(self.rows) - 1), self.top + 1)
            self.draw_screen()
        elif mouse_event.buttons & MouseButton.LEFT:
            target = self.top + mouse_event.cell_y - HEADER_ROWS
            if 0 <= target < len(self.rows) and self.rows[target][0] == 'i':
                self.sel = target
                self.choose()


def main(args: list[str]) -> dict[str, str] | None:
    try:
        sessions = fetch()
    except Exception as e:
        print(f'failed to list sessions: {e}')
        input('press enter')
        return None
    if not sessions:
        print(f'no zmx sessions on {HOST}')
        input('press enter')
        return None

    handler = Switcher(sessions)
    Loop().loop(handler)
    if handler.chosen is None:
        return None
    return {'session': handler.chosen.name, 'title': handler.chosen.tab_title}


@result_handler()
def handle_result(args: list[str], answer: dict[str, str] | None,
                  target_window_id: int, boss: Any) -> None:
    if not answer:
        return
    session, title = answer['session'], answer['title']

    # Already open in a tab? Just focus it.
    for tab in boss.all_tabs:
        for window in tab:
            if session in ' '.join(window.child.cmdline or ()):
                boss.set_active_tab(tab)
                return

    boss.call_remote_control(None, (
        'launch', '--type=tab', '--tab-title', title, '--title', title,
        '--cwd', os.path.expanduser('~'),
        SUPACODE, 'attach', session,
    ))


if __name__ == '__main__':
    raise SystemExit('This is a kitty kitten; run it via: kitten supacode_kitten.py')
