/* This code is copyright RAGE Software Inc 2013-2017. It may not be redistributed or modified in any way without prior written consent from RAGE Software. It may only be used with EverWeb built websites.
 */
function RssWidget(a) {
    void 0 == a.maxItems && (a.maxItems = 8), void 0 == a.descLimit && (a.descLimit = 100), void 0 == a.displayType && (a.displayType = 1), void 0 != a.refresh && ($("#trace").html("update" + updates), updates++);
    var Rss_url = a.feedUrl,
        Rss_domain = GetDomain(a.feedUrl),
        Rss_protocol = GetProtocol(a.feedUrl);
    Rss_url = encodeURIComponent(Rss_url), $("#" + a.divID).ready(function() {
        $.ajax({
            data: { num: a.maxItems, Rss_url:a.feedUrl, code:a.code},
            url: "/ew_PHP/ewRSSFeed.php",
            dataType: "json",
            type: "GET",
            beforeSend: function() {
                void 0 == a.refresh && $("#" + a.divID).empty()
            },
            success: function(c) {
                if (null == c.responseData && "feed:" == Rss_protocol) return a.feedUrl = a.feedUrl.replace("feed://", "http://"), void RssWidget(a);
                var d = "";
                itemCnt = "";
                var e = 0,
                    f = $("#" + a.divID).height() - 20,
                    g = $("#" + a.divID).width(),
                    h = c.responseData.feed.title;
                for (entries = c.responseData.feed.entries, i = 0; i < entries.length; i++) {
                    var b = c = "";
                    if (void 0 != a.showDate && (c = '<div class="date">' + showTime(entries[i].publishedDate) + "</div>"), item_link = entries[i].link, item_title = '<a href="' + item_link + '">' + entries[i].title + "</a>", item_image = "", item_content = entries[i].content, item_content = $("<div>" + item_content + "</div>").text(), entries[i].mediaGroups) {
                        item_hasHTML = entries[i].mediaGroups, void 0 != item_hasHTML && (item_image = entries[i].mediaGroups[0].contents[0].url);
                        var Img_domain = GetDomain(item_image);
                         if  (Img_domain != Rss_domain) {
                    		item_image = "", entries[i].mediaGroups[0].contents.length > 1 && (item_image = entries[i].mediaGroups[0].contents[1].url)
                    	 }
                    }
                    switch (a.showPostImg && "" == item_image && (item_image_src = $("<div>" + entries[i].content + "</div>").children("img").eq(0).attr("src"), void 0 == item_image_src && (item_image_src = $("<div>" + entries[i].content + "</div>").find("img:first").attr("src")), void 0 != item_image_src && (item_image = item_image_src, z = item_image_src.indexOf("pixel.wp.com"), z > 0 && (item_image = ""))), item_content = ShowDescription(item_content, a.descLimit), a.displayType) {
                        case 1:
                            itemCnt = '<div class="rsswidgetitem"><div class="title">' + item_title + "</div>" + c + '<div class="item_cnt"><div class="description">' + item_content + "</div></div></div>";
                            break;
                        case 2:
                            itemCnt = '<div class="rsswidgetitem img_top">', 10 < item_image.length && auth_img != item_image && (itemCnt += '<div class="image"><img src="' + item_image + '" ', void 0 != a.maxWidth && (itemCnt += ' width="' + a.maxWidth + '" '), itemCnt += ' alt="' + entries[i].title + '"></div>'), itemCnt += '<div class="title">' + item_title + "</div>" + c, itemCnt += '<div class="item_cnt">', itemCnt += '<div class="description">' + item_content + "</div>", itemCnt += '</div><div class="clear"></div></div>';
                            break;
                        case 3:
                            itemCnt = '<div class="rsswidgetitem img_left"><div class="title">' + item_title + "</div>" + c, itemCnt += '<div class="item_cnt">', 10 < item_image.length && auth_img != item_image && (itemCnt += '<span class="image"><img src="' + item_image + '" ', void 0 != a.maxWidth && (itemCnt += ' width="' + a.maxWidth + '" '), itemCnt += ' alt="' + entries[i].title + '" ></span>'), itemCnt += '<span class="description">' + item_content + "</span>", itemCnt += '</div><div class="clear"></div></div>';
                            break;
                        case 4:
                            itemCnt = '<div class="rsswidgetitem img_right"><div class="title">' + item_title + "</div>" + c, itemCnt += '<div class="item_cnt">', "" != item_image && auth_img != item_image && (itemCnt += '<span class="image"><img src="' + item_image + '" ', void 0 != a.maxWidth && (itemCnt += ' width="' + a.maxWidth + '" '), itemCnt += ' alt="' + entries[i].title + '" ></span>'), itemCnt += '<span class="description">' + item_content + "</span>", itemCnt += '</div><div class="clear"></div></div>';
                            break;
                        case 5:
                            10 < item_image.length && auth_img != item_image && (itemCnt = '<div class="rsswidgetitem img_only">' + c, itemCnt += '<a href="' + item_link + '" title="' + entries[i].title + '">', itemCnt += '<img src="' + item_image + '" ', itemCnt += ' width="98%" ', itemCnt += ' alt="' + entries[i].title + '" >', itemCnt += "</a>", itemCnt += '</div><div class="clear"></div>');
                            break;
                        case 6:
                            itemCnt = "", 10 < item_image.length && auth_img != item_image && (b = '<a href="' + item_link + '" title="' + entries[i].title + '" class="cImg', 0 == e && (b += " lft"), b += '">', b += '<img src="' + item_image + '" ', void 0 == a.maxWidth && (ww = $("#" + a.divID).width() - 58, ww = Math.round(ww / 2), a.maxWidth = ww), b += ' width="' + a.maxWidth + '" ', b += ' alt="' + entries[i].title + '" >', itemCnt = b += "</a>", 0 == e ? e = 1 : (e = 0, itemCnt += '<div class="clear"></div>'));
                            break;
                        case 7:
                            itemCnt = "", 10 < item_image.length && auth_img != item_image && (b = '<a href="' + item_link + '" title="' + entries[i].title + '" class="cImg', 3 == e && (e = 0), 2 > e && (b += " lft"), e++, b += '">', b += '<img src="' + item_image + '" ', void 0 == a.maxWidth && (ww = $("#" + a.divID).width() - 75, ww = Math.round(ww / 3), a.maxWidth = ww), b += ' width="' + a.maxWidth + '" ', b += ' alt="' + entries[i].title + '" >', itemCnt = b += "</a>", 3 == e && (itemCnt += '<div class="clear"></div>'))
                    }
                    auth_img != item_image && (auth_img = item_image), void 0 == a.refresh ? d += itemCnt : (t = $("#" + a.divID + " .rsswidget #rsswidgetCnt").html(), p = t.indexOf(entries[i].title), item = [itemId, itemCnt], 0 > p && Items[a.divID].unshift(item)), itemId++
                }
                if (void 0 == a.refresh) {
                    if ($("#" + a.divID).empty(), $("#" + a.divID).append('<div class="rsswidget" id="rsswidget"><div id="rsswidgetCnt"></div></div>'), $("#" + a.divID).append('<div class="rssLink"><a href="' + a.feedUrl + '">' + h + "</a></div>"), $("#" + a.divID + " .rssLink").width(g - 10), $("#" + a.divID + " .rsswidget #rsswidgetCnt").width(g - 10), $("#" + a.divID + " .rsswidget #rsswidgetCnt").append(d), $("#" + a.divID + " .rsswidget #rsswidgetCnt .rsswidgetitem").hide().fadeIn(), (6 == a.displayType || 7 == a.displayType) && ($("#" + a.divID + " .rsswidget").addClass("ItemColumns"), f -= 20), $("#" + a.divID + " .rsswidget").height(f + "px"), ns = $("#" + a.divID + " .rsswidget"), yScroll = new IScroll("#" + a.divID + " .rsswidget", {
                            scrollbars: !0,
                            mouseWheel: !0,
                            interactiveScrollbars: !0,
                            preventDefaultException: {
                                tagName: /^A$/
                            }
                        }), yScroll.refresh(), 1 == scrollRefresh) {
                        var k = setTimeout(function() {
                            yScroll.refresh(), clearTimeout(k)
                        }, 1e3);
                        scrollRefresh = 0
                    }
                    0 < a.updateInterval && (d = "interval_" + a.divID, uWidget = d + " = setTimeout(function(){ UpdateRSSWidget(a) }," + a.updateInterval + ")", eval(uWidget))
                } else void 0 != a.updateInterval && (d = "interval_" + a.divID, uWidget = d + " = setTimeout(function(){ UpdateRSSWidget(a) }," + a.updateInterval + ")", yScroll.refresh(), eval(uWidget))
            },
            error: function() {
                $("#" + a.divID).html('<div class="error">Error</div>')
            }
        })
    })
}

function UpdateRSSWidget(a) {
    yScroll.refresh(), uWidget = "clearTimeout(interval_" + a.divID + ")", eval(uWidget), a.refresh = 1, RssWidget(a)
}

function showTime(e) {
    var t = "",
        i = new Date;
    return today = i.setHours(0, 0, 0, 0), e = e.split(" "), s_time = e[1] + " " + e[2] + " " + e[3] + " " + e[4], t = e[4].split(":"), hours_v = t[0], 12 < hours_v ? (hours_v -= 12, hours_v += ":" + t[1] + " pm") : hours_v += ":" + t[1] + " am", t = e[2] + " " + e[1] + "," + e[3], art_time = new Date(s_time), art_time = art_time.setHours(0, 0, 0, 0), today == art_time && (t = "Today"), yesterday = i.setDate(i.getDate() - 1), yesterday == art_time && (t = "Yesterday"), t += ", " + hours_v
}

function ShowDescription(e, t) {
if (t == 0) {
        	return ""
        	}


    var i, a = 0,
        s = 0;
    for (i = e.split(/[\^\s\f\n\.,!;:{}()]/g), cnt = "", n = 0; n < i.length; n++)
        if (s = i[n].length, next = a += s, a += 1, 0 == s && (i[n] = " "), cnt += i[n], void 0 != e[next] && (cnt += e[next]), a > t) {
            last_chr = cnt.charAt(cnt.length - 1), (" " == last_chr || "," == last_chr) && (cnt += " ...");
            break
        }
    return cnt
}

function GetDomain(e) {
    var t = document.createElement("a");
    return domain = "", t.setAttribute("href", e), domain = t.hostname, t = null, domain
}

function GetProtocol(e) {
    var t = document.createElement("a");
    return domain = "", t.setAttribute("href", e), protocol = t.protocol, t = null, protocol
}
var itemId = updates = 1,
    Items = [],
    yScroll, startY, isStarted = !1,
    scrollRefresh = 1,
    auth_img = "";