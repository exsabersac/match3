package io.github.exsabersac.match3;

import android.os.Bundle;
import android.webkit.WebSettings;
import android.webkit.WebView;
import androidx.core.view.WindowCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.core.view.WindowInsetsControllerCompat;
import com.getcapacitor.BridgeActivity;

/**
 * 消消乐主界面：Capacitor 的 BridgeActivity（WebView 加载 https://localhost/ 上的网页版）。
 *
 * <p>这里只做壳层设置：沉浸式全屏（隐藏状态栏和导航栏，从边缘滑动临时呼出）、关闭 WebView 缩放、
 * 固定文字缩放。返回键在网页侧由 android-shim.js 通过 @capacitor/app 处理（弹「退出？」确认）。
 */
public class MainActivity extends BridgeActivity {

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        WebView webView = getBridge().getWebView();
        WebSettings s = webView.getSettings();
        // 禁止捏合 / 双击缩放（页面 viewport 也写了 user-scalable=no，这里在 WebView 层再关一次）
        s.setSupportZoom(false);
        s.setBuiltInZoomControls(false);
        s.setDisplayZoomControls(false);
        // 系统「字体大小」不影响画布里的字（画布自己按格子缩放字号），固定 100% 免得 DOM 提示文字错位
        s.setTextZoom(100);
        // 禁止长按选择 / 过度滚动光晕
        webView.setOverScrollMode(WebView.OVER_SCROLL_NEVER);
        webView.setHapticFeedbackEnabled(false);
        enterImmersive();
    }

    @Override
    public void onWindowFocusChanged(boolean hasFocus) {
        super.onWindowFocusChanged(hasFocus);
        // 从对话框 / 其他应用回来后系统栏可能重新出现，重新隐藏
        if (hasFocus) {
            enterImmersive();
        }
    }

    private void enterImmersive() {
        WindowInsetsControllerCompat c = WindowCompat.getInsetsController(getWindow(), getWindow().getDecorView());
        c.setSystemBarsBehavior(WindowInsetsControllerCompat.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE);
        c.hide(WindowInsetsCompat.Type.systemBars());
    }
}
