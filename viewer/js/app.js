/* ------------------------------------------------------------------------- */

$(document).ready( 
  function start ()
  {
    setupLoadingIcon()
    installCollapsers()
    installOptions()
    installEnters()
    installSmartFields()
    installHandlers()
    index()
    setupBrowserHistory()
    updateAuth()
  }
)

function index ()
{
  // When no document is requested using the resource hash, open the index
  // document by default.
  if (!document.location.hash)
    document.location.hash = 'Index'
}

function setupBrowserHistory ()
{
  // History changes are reflected using the parseURI/update functions.
  $.history.init(
    function historyChange (hash)
    {
      parseURI()
      update()
    }
  )
}

function setupLoadingIcon ()
{
  $("#loading").ajaxStart(
    function ()
    {
       $(this).show();
    }
  );
  $("#loading").ajaxStop(
    function ()
    {
       $(this).hide();
    }
  ); 
}

function installHandlers ()
{
  $('#history-btn').click(showHistory)
  $('#edit-btn').click(showEditor)
  $('#login-btn').click(login)
  $('#logout-btn').click(logout)
  $('#create-account-btn').click(signup)
  $('#save-btn').click(save)
  $('#preview-btn').click(preview)
  $('#rename-btn').click(rename)
  $('#delete-btn').click(_delete)
  $('#search-btn').click(search)
  $('#search-prev-btn').click(searchPrev)
  $('#search-next-btn').click(searchNext)
}

/* ------------------------------------------------------------------------- */

// Current application state.

Wiki = {

  // Static information.
    prefix     : 'data/'
  , previewURI : 'data/preview'
  , searchURI  : 'data/search'
  , loginURI   : 'login'
  , logoutURI  : 'logout'
  , loginfoURI : 'loginfo'
  , signupURI  : 'signup'
  , viewerURI  : '#'

  // State information.
  , title      : ''
  , ext        : 'html'
  , rev        : ''
  , anchor     : ''

  , prevTitle  : ''
  , prevExt    : ''
  , prevRev    : ''
  , prevAnchor : ''

  , author     : ''
  , user       : ''

  , search     : {
      results : []
    , max     : 20
    , index   : 0
  }
}

function parseURI ()
{
  var hash = window.location.hash
  var parse = hash.match(/^#([^?#]*)\??([^#]*)#?(.*)/)

  var ext = parse[1].match(/\.([^.]*)$/)
  var tit = parse[1].match(/(.*)\.([^.]*)$/)

  Wiki.prevTitle  = Wiki.title
  Wiki.prevExt    = Wiki.ext
  Wiki.prevRev    = Wiki.rev
  Wiki.prevAnchor = Wiki.anchor

  Wiki.title  = tit ? tit[1] : parse[1]
  Wiki.ext    = ext ? ext[1] : 'html'
  Wiki.rev    = parse[2]
  Wiki.anchor = parse[3]
}

function update ()
{
  var t = 0

  // Only refresh when one of these change.
  if (Wiki.title != Wiki.prevTitle ||
      Wiki.ext   != Wiki.prevExt   ||
      Wiki.rev   != Wiki.prevRev) {

    // Update information bar.
    $('#document').html(Wiki.title)
    $('#revision').html(Wiki.rev || '<i>head revision</i>')
    $('#user').html(Wiki.user)

    show()
    showHistory()
    updateAuth()

    // Anchor focus delay when refreshing.
    t = 1000
  }

  // if (Wiki.anchor)
    // setTimeout(
      // function ()
      // {
        followAnchor(Wiki.anchor)
      // }, t
    // )
}

function makeURI (prefix, title, ext, rev, anchor)
{
  var p = prefix === undefined ? Wiki.prefix : prefix
  var t = title  === undefined ? Wiki.title  : title
  var e = ext    === undefined ? Wiki.ext    : ext
  var r = rev    === undefined ? Wiki.rev    : rev
  var a = anchor === undefined ? Wiki.anchor : anchor
  return p + t
    + (e ? '.' + e : '')
    + (r ? '?' + r : '')
    + (a ? '#' + a : '')
}

function makeURIExt (ext)
{
  return makeURI(undefined, undefined, ext)
}

/* ---- showing documents -------------------------------------------------- */

function viewerHTML (txt)
{
  if (Wiki.ext == 'html') {
    $('#plain-viewer').hide()
    $('#plain-viewer').html('')
    $('#rich-viewer').show()
    $('#rich-viewer').html(txt)
  } else {
    $('#rich-viewer').hide()
    $('#rich-viewer').html('')
    $('#plain-viewer').show()
    $('#plain-viewer').html(txt)
  }
}

function setExternalLinks ()
{
  $('#txt-btn').   attr('href', makeURI(Wiki.viewerURI, undefined, 'txt'))
  $('#html-btn').  attr('href', makeURI(Wiki.viewerURI, undefined, 'html'))
  $('#latex-btn'). attr('href', makeURI(Wiki.viewerURI, undefined, 'tex'))
  $('#adt-btn').   attr('href', makeURI(Wiki.viewerURI, undefined, 'hs'))
  $('#pdf-btn').   attr('href', makeURIExt('pdf'))
  $('#print-btn'). attr('href', makeURIExt('html'))
  $('#src-btn').   attr('href', makeURIExt('txt'))
}

function show ()
{
  $.get(
      makeURIExt(Wiki.ext)
    , ''

    // Showing existing document:
    , function (res)
      {
        viewerHTML(res)
        $('#history-btn').removeClass('disabled')
        panelClose($('#edit-btn'), $('#editor'))
        processLinks()
        processImages()
        setExternalLinks()
      }
    , ''

    // `Showing' non-existing document:
    , function ()
      {
        viewerHTML('new document, click <strong>edit</strong> to start editing')
        $('#history-btn').addClass('disabled')
        showEditor() //todo
      }
  )
}

function processLinks ()
{
  $(".Internal").each(
    function ()
    {
      var href = this.getAttribute('href')
      if (!href.match(/:/))
        this.setAttribute('href', '#' + href)
    }
  )

  $(".Reference").each(
    function ()
    {
      $(this).attr('href', makeURI(Wiki.viewerURI, undefined, undefined, undefined, $(this).attr('href').substring(1)))
    }
  )
}

function processImages ()
{
  $("img").each(
    function ()
    {
      var src = this.getAttribute('src')
      this.setAttribute('src', src.replace(/^_/, 'data/_'))
    }
  )
}

function followAnchor (ref)
{
  var target = $('a[name=' + ref + ']')[0]
  target.scrollIntoView()

  // setTimeout(
    // function ()
    // {
      // focusElement(target.childeNodes[0])
    // }, 0)
}

// function focusElement (target)
// {
  // alert(target)
  // var div = document.createElement('div')
  // document.body.appendChild(div)
  // var bcr = target.getBoundingClientRect()
  // div.style.left   = bcr.left
  // div.style.top    = bcr.top
  // div.style.width  = target.clientWidth
  // div.style.height = target.clientHeight
  // $(div).addClass('find')
  // $(div).fadeOut('slow')
// }

/* ---- authentication bar ------------------------------------------------- */

function login ()
{
  if ($('#login-btn').hasClass('disabled'))
    return;

  $.post(
    Wiki.loginURI
  , { username : $('#username').val()
    , password : $('#password').val()
    } 
  , function ()
    {
      $('#login-status').removeClass('error')
      $('#login-status').html('login successful')
      panelClose($('#auth-btn'), $('#auth'))
      updateAuth()
    }
  , ''
  , function (e)
    {
      $('#login-status').addClass('error')
      $('#login-status').html('login failed: <span>' + e.responseText + '</span>')
    }
  )
}

function logout ()
{
  if ($('#logout-btn').hasClass('disabled'))
    return;

  $.post(
    Wiki.logoutURI
  , function ()
    {
      $('#login-status').removeClass('error')
      $('#login-status').html('logout successful')
      $('#auth').fadeOut()
      updateAuth()
    }
  )
}

function updateAuth ()
{
  $.get(
      Wiki.loginfoURI
    , ''
    , function (res)
      {
        var m = res.match(/username=(.*?\n)/)
        Wiki.user = (m && m[1]) || ''
        $('#user').html(Wiki.user)
        var bla = $('#user-ui').val(Wiki.user)
        fieldUpdate.call(bla)
      }
    , ''
    , function (res)
      {
        Wiki.user = ''
        $('#user').html('<i>not logged in</i>')
        fieldUpdate.call($('#user-ui').val(''))
      }
  )
}

function signup ()
{
  if ($('#signup-btn').hasClass('disabled'))
    return;

  $.post(
    Wiki.signupURI
  , { username : $('#su-username').val()
    , password : $('#su-password').val()
    , email    : $('#su-email').val()
    } 
  , function ()
    {
      $('#signup-status').removeClass('error')
      $('#signup-status').html('signup successful')
      panelClose($('#signup-btn'), $('#signup'))
    }
  , ''
  , function (e)
    {
      $('#signup-status').addClass('error')
      $('#signup-status').html('signup failed: <span>' + e.responseText + '</span>')
    }
  )
}

/* ---- document history --------------------------------------------------- */

function showHistory (title)
{
  // Refuse history updates when panel is closed.
  if (!$('#history').hasClass('open'))
    return

  $.get(makeURIExt('his'), '',
    function (res)
    {
      $('#history-results').html(historyTable(res))
      $('#history-status').removeClass('error')
      $('#history-status').html('')
    },
    '',
    function (e)
    {
      $('#history-results').html('')
      $('#history-status').addClass('error')
      $('#history-status').html('no history available: <span>' + e.responseText + '</span>')
    }
  )
}

function historyTable (res)
{
  function row (c, i)
  {
    var modified    = c.match( /(^|[\n])date=([^\n]*)/        )[2]
    var author      = c.match( /(^|[\n])author=([^\n]*)/      )[2]
    var email       = c.match( /(^|[\n])email=([^\n]*)/       )[2]
    var description = c.match( /(^|[\n])description=([^\n]*)/ )[2]
    var revid       = c.match( /(^|[\n])id=([^\n]*)/          )[2]

    // Side note.
    var klass =
      name == Wiki.rev ||
      (i == 0 && !Wiki.rev) 
      ? 'class="current" ' : ''

    return [
      , '<tr $klass>'
      , '<td>$modified</td>'
      , '<td>$author</td>'
      , '<td><a href="$uri">$description</a></td>'
      , '</tr>'
      ].join('').interpolate({
          'uri'         : makeURI(Wiki.viewerURI, undefined, undefined, description)
        , 'klass'       : klass
        , 'modified'    : modified
        , 'author'      : author
        , 'description' : description
      })
  }

  function col (c)
  {
    return '<td>' + c + '</td>'
  }

  return '' + 
    '<table cellpadding="0" cellspacing="0"><tr>' +
    '<th id="modified">modified</th>' +
    '<th id="author">author</th>' +
    '<th id="changename">change</th>' +
    '</tr>' +
    res.asHTML().split("\n\n").map(row).join('')
    + '</tbody></table>'
}

/* ---- document editor ---------------------------------------------------- */

function showEditor (title)
{
  $.get(makeURIExt('txt'), '',

    // Editing an existing document:
    function (res)
    {
      $('#editField').val(res)
    },
    '',

    // `Editing' a new document:
    function (res)
    {
      $('#editField').val('<new document>')
    }
  )
}

function save (title)
{
  if (!Wiki.user)
    return;

  $.put(
    makeURI(undefined, undefined, 'txt', $('#change.full').val())
  , $('#editField').val()
  , function ()
    {
      $('#edit-status').removeClass('error')
      $('#edit-status').html('save successful')
      panelClose($('#edit-btn'), $('#editor'))
      show()
      showHistory()
    }
  , ''
  , function (e)
    {
      $('#edit-status').addClass('error')
      $('#edit-status').html('save failed: <span>' + e.responseText + '</span>')
    }
  )
}

function preview ()
{
  $.put(
    Wiki.previewURI + '.' + Wiki.ext
  , $('#editField').val()
  , function (res)
    {
      viewerHTML(res)
      processLinks()
      processImages()
      $('#edit-status').removeClass('error')
      $('#edit-status').html('preview successful')
    }
  )
}

function rename ()
{
  $._delete(
    makeURI(undefined, undefined, '', $('#change.full').val())
  , $('#rename').val()
  , function (res)
    {
      $('#edit-status').removeClass('error')
      $('#edit-status').html('rename successful')
      document.location.hash = $('#rename').val()
    }
  )
}

function _delete ()
{
  $._delete(
    makeURI(undefined, undefined, '', $('#change.full').val())
  , ''
  , function (res)
    {
      $('#edit-status').removeClass('error')
      $('#edit-status').html('delete successful')
      document.location.hash = ''
    }
  )
}

/* ---- search bar --------------------------------------------------------- */

function search ()
{
  if ($('#search-btn').hasClass('disabled'))
    return;

  $('#search-status').removeClass('error')
  $('#search-status').html('searching...')

  $.post(
    Wiki.searchURI
  , { patterns   : $('#search-terms').val()
    , wholewords : 'False'
    , matchall   : 'False'
    , ignorecase : $('#search-case:checked').length == 0 ? 'True' : 'False'
    }
  , function (res)
    {
      var rs = Wiki.search.results = res.asHTML().split("\n\n")
      Wiki.search.index = 0

      if (!rs || rs.length == 0 || (rs.length == 1 && !rs[0])) {
        $('#search-status').addClass('error')
        $('#search-status').html('no search results')
        searchPrevReset()
        searchNextReset()
        $('#search-results').html('')
      } else {
        $('#search-status').removeClass('error')
        $('#search-status').html('search successful (' + rs.length + ' results)')
        $('#search-results').html(searchTable())
      }
    }
  , ''
  , function (e)
    {
      $('#search-status').addClass('error')
      $('#search-status').html('search failed: <span>' + e.responseText + '</span>')
    }
  )

}

function searchPrevReset ()
{
  $('#search-prev-btn').addClass('disabled')
  $('#search-prev-btn').html('previous results')
}

function searchNextReset ()
{
  $('#search-next-btn').addClass('disabled')
  $('#search-next-btn').html('next results')
}

function searchNext ()
{
  if ($('#search-next-btn').hasClass('disabled'))
    return;

  Wiki.search.index += Wiki.search.max
  $('#search-results').html(searchTable())
}

function searchPrev ()
{
  if ($('#search-prev-btn').hasClass('disabled'))
    return;

  Wiki.search.index -= Wiki.search.max
  $('#search-results').html(searchTable())
}

function searchTable ()
{
  function row (c, i)
  {
    var resource   = c.match( /(^|[\n])resource=([^\n]*)/   )[2]
    var linenumber = c.match( /(^|[\n])linenumber=([^\n]*)/ )[2]
    var line       = c.match( /(^|[\n])line=([^\n]*)/       )[2]

    return [
        '<tr>'
      , '<td><a href="$uri">$resource</a></td>'
      , '<td>$linenumber</td>'
      , '<td>$line</td>'
      , '</tr>'
      ].join('').interpolate({
          'uri'        : makeURI(Wiki.viewerURI, resource, undefined, '')
        , 'resource'   : resource
        , 'linenumber' : linenumber
        , 'line'       : line
      })
  }

  function col (c)
  {
    return '<td>' + c + '</td>'
  }

  var rs      = Wiki.search.results
      ix      = Wiki.search.index
      max     = Wiki.search.max
      prevIx  = Math.max(0,             ix - max) 
      prevMax = Math.max(0,             ix) 
      nextIx  = Math.min(rs.length - 1, ix + max)
      nextMax = Math.min(rs.length - 1, ix + max + max)

  if (prevMax > 0) {
    $('#search-prev-btn').removeClass('disabled')
    $('#search-prev-btn').html('previous results <span>(' + prevIx + '-' + prevMax + ')</span>')
  } else
    searchPrevReset()

  if (nextIx < rs.length - 1) {
    $('#search-next-btn').removeClass('disabled')
    $('#search-next-btn').html('next results <span>(' + nextIx + '-' + nextMax + ')</span>')
  } else
    searchNextReset()

  return '' + 
      '<table cellpadding="0" cellspacing="0"><tr>'
    + '<th id="resource">resource</th>'
    + '<th id="linenumber">line</th>'
    + '<th id="line">match</th>'
    + '</tr>'
    + rs.slice(ix, ix + max).map(row).join('')
    + '</tbody></table>'
}

