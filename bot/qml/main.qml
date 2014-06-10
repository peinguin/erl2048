import QtQuick 2.0

Rectangle {
    width: 480;
    height: 640
    Row {
        y: 10;
        x: 10
        Rectangle{
            width: 200;
            Text {
                width: 100;
                font.pixelSize: 30;
                text: "Score:";
                }
            Text {
                x: 100;
                width: 140;
                font.pixelSize: 30;
                text: score;
                }
            }
        Rectangle{
            x: 240;
            Text {
                width: 80;
                font.pixelSize: 30;
                text: "Best:";
                }
            Text {
                x: 80;
                width: 160;
                font.pixelSize: 30;
                text: best;
                }
            }
    }
}