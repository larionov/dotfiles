import QtQuick
import Quickshell
import Quickshell.Io
import qs.Common
import qs.Services
import qs.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    property string currentMode: "default"
    readonly property bool isVmMode: currentMode === "vm"

    // Get initial state on load, then watch for changes
    Process {
        id: mmsgInit
        command: ["mmsg", "-g", "-b"]
        running: true

        stdout: SplitParser {
            onRead: data => {
                const parts = data.trim().split(/\s+/)
                if (parts.length >= 3 && parts[1] === "keymode") {
                    root.currentMode = parts[2]
                }
            }
        }
    }

    Process {
        id: mmsgWatcher
        command: ["mmsg", "-w", "-b"]
        running: true

        stdout: SplitParser {
            onRead: data => {
                const parts = data.trim().split(/\s+/)
                if (parts.length >= 3 && parts[1] === "keymode") {
                    root.currentMode = parts[2]
                }
            }
        }
    }

    // Click to toggle VM mode
    pillClickAction: function() {
        Quickshell.execDetached(["/home/larionov/.local/bin/toggle-vm-mode"])
    }

    horizontalBarPill: Component {
        Item {
            implicitWidth: hRow.implicitWidth + Theme.spacingS * 2
            implicitHeight: root.widgetThickness

            Row {
                id: hRow
                anchors.centerIn: parent
                spacing: Theme.spacingXS

                DankIcon {
                    anchors.verticalCenter: parent.verticalCenter
                    name: root.isVmMode ? "keyboard_hide" : "keyboard"
                    size: root.iconSize
                    color: root.isVmMode ? Theme.error : Theme.surfaceVariantText
                }
            }
        }
    }

    verticalBarPill: Component {
        Item {
            implicitWidth: root.widgetThickness
            implicitHeight: vIcon.size + Theme.spacingS * 2

            DankIcon {
                id: vIcon
                anchors.centerIn: parent
                name: root.isVmMode ? "keyboard_hide" : "keyboard"
                size: root.iconSize
                color: root.isVmMode ? Theme.error : Theme.surfaceVariantText
            }
        }
    }
}
