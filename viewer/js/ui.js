/* ------------------------------------------------------------------------- */

function installCollapsers ()
{
  $('.collapser').click(togglePanel)
}

function togglePanel (show)
{
  var button = $(this)
  var panel  = $(button.attr('target'))

  if (button.hasClass('open') && show !== true)
    panelClose(button, panel)
  else if (show !== false)
    panelOpen(button, panel)
}

function panelClose (button, panel)
{
  panel.fadeOut()
  button.removeClass('open')
  panel.removeClass('open')
}

function panelOpen (button, panel)
{
  panel.addClass('open')
  button.addClass('open')
  panel.fadeIn()
}

function showPanel ()
{
  togglePanel.call(this, true)
}

function hidePanel ()
{
  togglePanel.call(this, false)
}

/* ------------------------------------------------------------------------- */

function installOptions()
{
  $('.option').click(setOpted)
}

function setOpted ()
{
  var button = $(this)
  var all    = $(this.parentNode).children()

  all.removeClass('opted')
  button.addClass('opted')
}

/* ------------------------------------------------------------------------- */

function installEnters ()
{
  $('input[enter]').keyup(
    function(e) {
      if(e.keyCode == 13)
        window[$(this).attr('enter')]()
    }
  )
}

/* ------------------------------------------------------------------------- */

function installSmartFields ()
{
  $('input')
    .focus(fieldFocus)
    .blur(fieldBlur)
    .change(fieldUpdate)
    .keyup(fieldUpdate)
    .map(fieldUpateTarget)
}

function fieldFocus ()
{
  var input = $(this)

  // Empty the field when it has no user content.
  if (!input.hasClass('full'))
    input.val('')
}

function fieldUpdate ()
{
  var input = $(this)

  input.val()
    ? input.addClass('full')
    : input.removeClass('full')

  fieldUpateTarget.call(input)
}

function fieldBlur ()
{
  // Restore description text when still/again empty.
  var input = $(this)
  if (!input.val())
    input.val(input.attr('name'))
}

function fieldUpateTarget ()
{
  var target = $(this).attr('target')
  var others = $('input')

  if (!target)
    return
  var targets = target.split(/,\s*/)
  if (!targets.length)
    return

  // For all separate targets.
  for (var t = 0; t < targets.length; t++) {

  // Get all non-full inputs with same target.
    var empty = others.filter(
      function (a)
      {
        var elem = $(others[a])
        return elem.attr('target')
           &&  elem.attr('target').match(targets[t])
           && !elem.hasClass('full')
      }
    )

    // Set enabled/disabled of target based amount of non-full inputs.
    if (empty.length)
      $(targets[t]).addClass('disabled')
    else
      $(targets[t]).removeClass('disabled')
  }

  return true;
}

/* ------------------------------------------------------------------------- */

