import QtQuick 2.0
import "main.js" as Main
Rectangle {
    id: app
    width: 450; height: 495
    color: "#888888"
    focus: true

    property variant numbers: []
    property int cols: 4
    property int rows: 4

    function purge() {
        var tmp = numbers
        for (var i = 0; i < tmp.length; i++) {
            tmp[i].destroy()
        }
        tmp = Array()
        numbers = tmp
    }

    function victory() {
        message.show("You win. Finally.")
    }
    function defeat() {
        message.show("May I suggest some Burial?")
    }

    Component {
        id: number

        Rectangle {
            id: colorRect
            color: number <=    1 ? "transparent" :
                   number <=    2 ? "#eee4da" :
                   number <=    4 ? "#ede0c8" :
                   number <=    8 ? "#f2b179" :
                   number <=   16 ? "#f59563" :
                   number <=   32 ? "#f67c5f" :
                   number <=   64 ? "#f65e3b" :
                   number <=  128 ? "#edcf72" :
                   number <=  256 ? "#edcc61" :
                   number <=  512 ? "#edc850" :
                   number <= 1024 ? "#edc53f" :
                   number <= 2048 ? "#edc22e" :
                                    "#3c3a32"

            property int colStart
            property int rowStart

            property int colEnd
            property int rowEnd

            property int scale

            property int number

            x:      cells.getAt(colStart, rowStart).x
            y:      cells.getAt(colStart, rowStart).y
            width:  cells.getAt(colStart, rowStart).width
            height: cells.getAt(colStart, rowStart).height
            radius: cells.getAt(colStart, rowStart).radius

            Text {
                id: text

                width: parent.width * 0.9
                height: parent.height * 0.9
                anchors.centerIn: parent

                font.family: "monospace"
                font.bold: true
                font.pixelSize: parent.height
                fontSizeMode: Text.Fit
                horizontalAlignment: Text.AlignHCenter
                verticalAlignment: Text.AlignVCenter

                text: parent.number > 1 ? parent.number : ""
            }


            transform: [
                Scale {
                    id: zoomIn
                    xScale: scale
                    yScale: scale
                    origin.x: colorRect.width / 2
                    origin.y: colorRect.height / 2
                    Behavior on xScale {
                        NumberAnimation {
                            duration: 300
                            easing {
                                type: Easing.InOutQuad
                            }
                        }
                    }
                    Behavior on yScale {
                        NumberAnimation {
                            duration: 300
                            easing {
                                type: Easing.InOutQuad
                            }
                        }
                    }
                },

                Translate{
                    id: translate
                    x: 0
                    y: 0
                    Behavior on x {
                        NumberAnimation {
                            duration: 500
                            easing {
                                type: Easing.InOutQuad
                            }
                        }
                    }
                    Behavior on y {
                        NumberAnimation {
                            duration: 500
                            easing {
                                type: Easing.InOutQuad
                            }
                        }
                    }
                }
            ]
            Component.onCompleted: {
                var cell = cells.getAt(colEnd, rowEnd)
                zoomIn.xScale = 1
                zoomIn.yScale = 1
                translate.x = cell.x - this.x
                translate.y = cell.y - this.y
            }
        }
    }

    Rectangle {
        anchors.centerIn: parent
        color: "transparent"
        height: parent.height / 32 * 31
        width: parent.width / 32 * 31
        Rectangle {
            id: scorePanelRect
            anchors.top: parent.top
            height: parent.height * 0.1
            width: parent.width
            opacity: 0.66
            color: "white"
            radius: 2
            z: 2
            Text {
                id: scorePanel
                width: parent.width / 32 * 31
                height: parent. height / 32 * 31
                anchors.centerIn: parent

                font.pixelSize: height * 0.5
                horizontalAlignment: Text.AlignRight
                verticalAlignment: Text.AlignVCenter

                text: "Score: " + score;
            }
            Text {
                id: bestPanel
                width: parent.width / 32 * 31
                height: parent. height / 32 * 31
                anchors.centerIn: parent

                font.pixelSize: height * 0.5
                horizontalAlignment: Text.AlignLeft
                verticalAlignment: Text.AlignVCenter

                text: "Best: " + best;
            }
        }
        Grid {
            id: cellGrid
            width: parent.width
            height: (parent.height - scorePanel.height) / 32 * 31
            anchors.bottom: parent.bottom
            rows: app.rows
            columns: app.cols
            spacing: (parent.width + parent.height) / app.rows / app.cols / 4

            property real cellWidth: (width - (columns - 1) * spacing) / columns
            property real cellHeight: (height - (rows - 1) * spacing) / rows

            Repeater {
                id: cells
                model: app.cols * app.rows
                function getAt(h, v) {
                    return itemAt(h + v * app.cols)
                }
                Rectangle {
                    width: parent.cellWidth
                    height: parent.cellHeight
                    color: "#AAAAAA"
                    radius: 2

                    property int col : index % app.cols
                    property int row : index / app.cols
                }
            }
        }
        Rectangle {
            id: message
            width: app.width
            height: app.height
            anchors.centerIn: parent
            opacity: 0.0
            color: "black"
            visible: false
            z: 1
            function hide() {
                visible = false
                opacity = 0.0
                messageText.text = ""
            }
            function show(text) {
                visible = true
                opacity = scorePanelRect.opacity
                messageText.text = text
            }
            Rectangle {
                anchors.centerIn: parent
                width: parent.width * 0.66
                height: parent.height * 0.33
                color: "black"
                Rectangle {
                    anchors.fill: parent
                    width: parent.width - 2
                    height: parent.height -.2
                    color: "white"
                    Text {
                        anchors.fill: parent
                        id: messageText
                        font.pixelSize: parent.height * 0.13
                        anchors.centerIn: parent
                        horizontalAlignment: Text.AlignHCenter
                        verticalAlignment: Text.AlignVCenter
                    }
                }
            }
            Behavior on opacity { 
                NumberAnimation {
                    duration: 200
                }
            }
        }
    }

    Component.onCompleted: {
        updateSignal.connect(Main.processor);
    }
}
