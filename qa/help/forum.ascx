<%@ Control Language="c#" Codebehind="forum.ascx.cs" AutoEventWireup="True" Inherits="yaf.pages.forum" %>
<%@ Register TagPrefix="yaf" Namespace="yaf.controls" Assembly="yaf" %>
<%@ Register TagPrefix="yaf" TagName="ForumList" Src="../controls/ForumList.ascx" %>
<yaf:PageLinks runat="server" ID="PageLinks" />
<div id="Welcome" runat="server">
    <div id="divTimeNow">
        <asp:Label ID="TimeNow" runat="server" /></div>
    <div id="divTimeLastVisit">
        <asp:Label ID="TimeLastVisit" runat="server" /></div>
    <div id="divUnreadMsgs">
        <asp:HyperLink runat="server" ID="UnreadMsgs" Visible="false" /></div>
</div>
<br />
<asp:Repeater ID="CategoryList" runat="server" OnItemCommand="categoryList_ItemCommand"
    OnItemDataBound="CategoryList_ItemDataBound">
    <HeaderTemplate>
        <table class="content" cellspacing="1" cellpadding="0" width="100%">
            <tr class="header1">
                <td width="1%">
                    &nbsp;</td>
                <td align="left" class="header1">
                    <%# GetText("FORUM") %>
                </td>
                <td class="header1" align="center" width="7%">
                    <%# GetText("topics") %>
                </td>
                <td class="header1" align="center" width="7%">
                    <%# GetText("posts") %>
                </td>
                <td class="header1" align="center" width="25%">
                    <%# GetText("lastpost") %>
                </td>
            </tr>
    </HeaderTemplate>
    <ItemTemplate>
        <tr>
            <td class="header2" colspan="5">
                <asp:ImageButton runat="server" ID="expandCategory" BorderWidth="0" ImageAlign="Baseline"
                    CommandName="panel" CommandArgument='<%# DataBinder.Eval(Container.DataItem, "CategoryID") %>'>
                </asp:ImageButton>
                &nbsp;&nbsp; <a href='<%# yaf.Forum.GetLink(yaf.Pages.forum,"c={0}",DataBinder.Eval(Container.DataItem, "CategoryID")) %>'>
                    <%# DataBinder.Eval(Container.DataItem, "Name") %>
                </a>
            </td>
        </tr>
        <yaf:ForumList runat="server" Visible="true" ID="forumList" DataSource='<%# ((System.Data.DataRowView)Container.DataItem).Row.GetChildRows("FK_Forum_Category") %>' />
    </ItemTemplate>
    <FooterTemplate>
        </table>
    </FooterTemplate>
</asp:Repeater>
<table border="0" class="content" cellspacing="1" cellpadding="0" width="100%">
    <tr class="header1">
        <td colspan="2" class="header1">
            <asp:ImageButton runat="server" ID="expandActiveDiscussions" BorderWidth="0" ImageAlign="Baseline"
                OnClick="expandActiveDiscussions_Click" />&nbsp;&nbsp;<%= GetText("ACTIVE_DISCUSSIONS") %></td>
    </tr>
    <asp:PlaceHolder ID="ActiveDiscussionHolder" runat="server">
        <tr>
            <td class="header2" colspan="2">
                <%= GetText("LATEST_POSTS") %>
            </td>
        </tr>
        <asp:Repeater runat="server" ID="LatestPosts">
            <ItemTemplate>
                <tr>
                    <td class="post">
                        &nbsp;<b><a href='<%#yaf.Forum.GetLink(yaf.Pages.posts,"m={0}#{0}",DataBinder.Eval(Container.DataItem, "LastMessageID"))%>'><%# yaf.Utils.BadWordReplace(Convert.ToString(DataBinder.Eval(Container.DataItem, "Topic"))) %></a></b>
                        <%# String.Format( GetText( "BY" ), HtmlEncode(DataBinder.Eval( Container.DataItem, "LastUserName" ).ToString()) )%>
                        (<a href='<%#yaf.Forum.GetLink(yaf.Pages.topics,"f={0}",DataBinder.Eval(Container.DataItem, "ForumID"))%>'><%# Convert.ToString( DataBinder.Eval( Container.DataItem, "Forum" ) )%></a>)
                        	
						
						                
                    </td>
                    <td class="post" style="width: 25em; text-align: right;">
                        <%# FormatDateTimeTopic( Convert.ToDateTime( DataBinder.Eval( Container.DataItem, "LastPosted" ) ) )%>
                        <a href="<%#yaf.Forum.GetLink(yaf.Pages.posts,"m={0}#{0}",DataBinder.Eval(Container.DataItem, "LastMessageID"))%>">
                            <img src="<%# GetThemeContents("ICONS","ICON_LATEST") %>" border="0" alt=""></a>
                    </td>
                </tr>
            </ItemTemplate>
        </asp:Repeater>
    </asp:PlaceHolder>
</table>
<table class="content" cellspacing="1" cellpadding="0" width="100%">
    <tr class="header1">
        <td colspan="2" class="header1">
            <asp:ImageButton runat="server" ID="expandInformation" BorderWidth="0" ImageAlign="Baseline"
                OnClick="expandInformation_Click" />&nbsp;&nbsp;<%= GetText("INFORMATION") %></td>
    </tr>
    <asp:PlaceHolder ID="InformationHolder" runat="server">
        <tr>
            <td class="header2" colspan="2">
                <%= GetText("ACTIVE_USERS") %>
            </td>
        </tr>
        <tr>
            <td class="post" width="1%">
                <img src="<%# GetThemeContents("ICONS","FORUM_USERS") %>" alt="" /></td>
            <td class="post">
                <asp:Label runat="server" ID="activeinfo" /><br />
                <asp:Repeater runat="server" ID="ActiveList">
                    <ItemTemplate>
                        <a href='<%#yaf.Forum.GetLink(yaf.Pages.profile,"u={0}",DataBinder.Eval(Container.DataItem, "UserID"))%>'>
                            <%# Server.HtmlEncode(Convert.ToString(DataBinder.Eval(Container.DataItem, "Name"))) %>
                        </a>
                    </ItemTemplate>
                    <SeparatorTemplate>
                        ,
                    </SeparatorTemplate>
                </asp:Repeater>
            </td>
        </tr>
        <tr>
            <td class="header2" colspan="2">
                <%= GetText("STATS") %>
            </td>
        </tr>
        <tr>
            <td class="post" width="1%">
                <img src="<%# GetThemeContents("ICONS","FORUM_STATS") %>" alt="" /></td>
            <td class="post">
                <asp:Label ID="Stats" runat="server">Label</asp:Label></td>
        </tr>
    </asp:PlaceHolder>
</table>
<table width="100%" class="iconlegend">
    <tr>
        <td>
            <img align="middle" src="<% =GetThemeContents("ICONS","FORUM_NEW") %>" alt="" />
            <%# GetText("ICONLEGEND","New_Posts") %>
            <img align="middle" src="<% =GetThemeContents("ICONS","FORUM") %>" alt="" />
            <%# GetText("ICONLEGEND","No_New_Posts") %>
            <img align="middle" src="<% =GetThemeContents("ICONS","FORUM_LOCKED") %>" alt="" />
            <%# GetText("ICONLEGEND","Forum_Locked") %>
        </td>
        <td align="right">
            <asp:LinkButton runat="server" OnClick="MarkAll_Click" ID="MarkAll" /></td>
    </tr>
</table>
<yaf:SmartScroller ID="SmartScroller1" runat="server" />
