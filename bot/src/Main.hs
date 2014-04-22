import qualified Network.WebSockets as WS
import App.Main

main :: IO ()
main = WS.runClient "2048.php.poltava.ua" 8080 "/" app