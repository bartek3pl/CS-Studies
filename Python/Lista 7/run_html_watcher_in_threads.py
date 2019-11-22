import threading
from html_watcher import HtmlWatcher


class RunHtmlWatcherInThreads():

    def __init__(self, html_watcher):
        self.html_watchers = html_watchers
        self.threads = []

    def run(self):
        for i in range(len(self.html_watchers)):
            html_watcher = HtmlWatcher()
            t = threading.Thread(target=html_watcher.watch_HTML, args=[
                html_watchers[i][0], html_watchers[i][1], html_watchers[i][2]])
            t.start()
            self.threads.append(t)

        for thread in self.threads:
            thread.join()
        return ""


html_watchers = [
    ["bartek3pl.github.io/Github-Users-Finder/", 4, 24],
    ["geeksforgeeks.org/", 5, 15]
]

html_watcher_in_threads = RunHtmlWatcherInThreads(html_watchers)
html_watcher_in_threads.run()
