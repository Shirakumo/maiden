#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-irc-events
  (:nicknames #:irc #:org.shirakumo.maiden.clients.irc.events)
  (:use)
  ;; commands.lisp
  (:export
   #:connect
   #:disconnect
   #:pass
   #:nick
   #:user
   #:server
   #:oper
   #:quit
   #:squit
   #:join
   #:part
   #:mode
   #:topic
   #:names
   #:list
   #:invite
   #:kick
   #:version
   #:stats
   #:links
   #:time
   #:trace
   #:admin
   #:info
   #:privmsg
   #:notice
   #:who
   #:whois
   #:whowas
   #:ping
   #:pong
   #:error
   #:away
   #:rehash
   #:restart
   #:summon
   #:users
   #:wallops
   #:userhost
   #:ison)
  ;; events.lisp
  (:export
   #:msg-pass
   #:msg-nick
   #:msg-user
   #:msg-server
   #:msg-oper
   #:msg-quit
   #:msg-squit
   #:msg-join
   #:msg-part
   #:msg-mode
   #:msg-topic
   #:msg-names
   #:msg-list
   #:msg-invite
   #:msg-kick
   #:msg-version
   #:msg-stats
   #:msg-links
   #:msg-time
   #:msg-connect
   #:msg-trace
   #:msg-admin
   #:msg-info
   #:msg-privmsg
   #:msg-notice
   #:msg-who
   #:msg-whois
   #:msg-whowas
   #:msg-kill
   #:msg-ping
   #:msg-pong
   #:msg-error
   #:msg-away
   #:msg-rehash
   #:msg-restart
   #:msg-summon
   #:msg-users
   #:msg-wallops
   #:msg-userhost
   #:msg-ison
   #:rpl-welcome
   #:rpl-yourhost
   #:rpl-created
   #:rpl-myinfo
   #:rpl-bounce
   #:rpl-map
   #:rpl-mapend
   #:rpl-snomask
   #:rpl-statmemtot
   #:rpl-yourcookie
   #:rpl-map
   #:rpl-mapmore
   #:rpl-mapend
   #:rpl-yourid
   #:rpl-savenick
   #:rpl-attemptingjunc
   #:rpl-attemptingreroute
   #:rpl-tracelink
   #:rpl-traceconnecting
   #:rpl-tracehandshake
   #:rpl-traceunknown
   #:rpl-traceoperator
   #:rpl-traceuser
   #:rpl-traceserver
   #:rpl-traceservice
   #:rpl-tracenewtype
   #:rpl-traceclass
   #:rpl-tracereconnect
   #:rpl-statslinkinfo
   #:rpl-statscommands
   #:rpl-statscline
   #:rpl-statsnline
   #:rpl-statsiline
   #:rpl-statskline
   #:rpl-statsqline
   #:rpl-statsyline
   #:rpl-endofstats
   #:rpl-umodeis
   #:rpl-statsqline
   #:rpl-serviceinfo
   #:rpl-endofservices
   #:rpl-service
   #:rpl-servlist
   #:rpl-servlistend
   #:rpl-statsverbose
   #:rpl-statsengine
   #:rpl-statsfline
   #:rpl-statsiauth
   #:rpl-statslline
   #:rpl-statsuptime
   #:rpl-statsoline
   #:rpl-statshline
   #:rpl-statssline
   #:rpl-statsdline
   #:rpl-luserclient
   #:rpl-luserop
   #:rpl-luserunknown
   #:rpl-luserchannels
   #:rpl-luserme
   #:rpl-adminme
   #:rpl-adminloc1
   #:rpl-adminloc2
   #:rpl-adminemail
   #:rpl-tracelog
   #:rpl-traceend
   #:rpl-tryagain
   #:rpl-localusers
   #:rpl-globalusers
   #:rpl-start-netstat
   #:rpl-netstat
   #:rpl-end-netstat
   #:rpl-privs
   #:rpl-silelist
   #:rpl-endofsilelist
   #:rpl-notify
   #:rpl-statsdline
   #:rpl-vchanexist
   #:rpl-vchanlist
   #:rpl-vchanhelp
   #:rpl-glist
   #:rpl-chaninfo-kicks
   #:rpl-end-chaninfo
   #:rpl-none
   #:rpl-away
   #:rpl-userhost
   #:rpl-ison
   #:rpl-text
   #:rpl-unaway
   #:rpl-nowaway
   #:rpl-whoisuser
   #:rpl-whoisserver
   #:rpl-whoisoperator
   #:rpl-whowasuser
   #:rpl-endofwho
   #:rpl-whoischanop
   #:rpl-whoisidle
   #:rpl-endofwhois
   #:rpl-whoischannels
   #:rpl-liststart
   #:rpl-list
   #:rpl-listend
   #:rpl-channelmodeis
   #:rpl-uniqopis
   #:rpl-nochanpass
   #:rpl-chpassunknown
   #:rpl-channel-url
   #:rpl-creationtime
   #:rpl-notopic
   #:rpl-topic
   #:rpl-topicwhotime
   #:rpl-whoisbot
   #:rpl-badchanpass
   #:rpl-userip
   #:rpl-inviting
   #:rpl-summoning
   #:rpl-invited
   #:rpl-invitelist
   #:rpl-endofinvitelist
   #:rpl-exceptlist
   #:rpl-endofexceptlist
   #:rpl-version
   #:rpl-whoreply
   #:rpl-namreply
   #:rpl-whospcrpl
   #:rpl-namreply-
   #:rpl-map
   #:rpl-mapmore
   #:rpl-mapend
   #:rpl-killdone
   #:rpl-closing
   #:rpl-closeend
   #:rpl-links
   #:rpl-endoflinks
   #:rpl-endofnames
   #:rpl-banlist
   #:rpl-endofbanlist
   #:rpl-endofwhowas
   #:rpl-info
   #:rpl-motd
   #:rpl-infostart
   #:rpl-endofinfo
   #:rpl-motdstart
   #:rpl-endofmotd
   #:rpl-youreoper
   #:rpl-rehashing
   #:rpl-youreservice
   #:rpl-myportis
   #:rpl-notoperanymore
   #:rpl-alist
   #:rpl-endofalist
   #:rpl-time
   #:rpl-usersstart
   #:rpl-users
   #:rpl-endofusers
   #:rpl-nousers
   #:rpl-hosthidden
   #:err-unknownerror
   #:err-nosuchnick
   #:err-nosuchserver
   #:err-nosuchchannel
   #:err-cannotsendtochan
   #:err-toomanychannels
   #:err-wasnosuchnick
   #:err-toomanytargets
   #:err-nosuchservice
   #:err-noorigin
   #:err-norecipient
   #:err-notexttosend
   #:err-notoplevel
   #:err-wildtoplevel
   #:err-badmask
   #:err-lengthtruncated
   #:err-unknowncommand
   #:err-nomotd
   #:err-noadmininfo
   #:err-fileerror
   #:err-noopermotd
   #:err-toomanyaway
   #:err-eventnickchange
   #:err-nonicknamegiven
   #:err-erroneusnickname
   #:err-nicknameinuse
   #:err-nickcollision
   #:err-unavailresource
   #:err-targettoofast
   #:err-servicesdown
   #:err-usernotinchannel
   #:err-notonchannel
   #:err-useronchannel
   #:err-nologin
   #:err-summondisabled
   #:err-usersdisabled
   #:err-nonickchange
   #:err-notimplemented
   #:err-notregistered
   #:err-idcollision
   #:err-nicklost
   #:err-hostilename
   #:err-acceptfull
   #:err-acceptexist
   #:err-acceptnot
   #:err-nohiding
   #:err-notforhalfops
   #:err-needmoreparams
   #:err-alreadyregistered
   #:err-nopermforhost
   #:err-passwdmismatch
   #:err-yourebannedcreep
   #:err-youwillbebanned
   #:err-keyset
   #:err-linkset
   #:err-channelisfull
   #:err-unknownmode
   #:err-inviteonlychan
   #:err-bannedfromchan
   #:err-badchannelkey
   #:err-badchanmask
   #:err-nochanmodes
   #:err-banlistfull
   #:err-noprivileges
   #:err-chanoprivsneeded
   #:err-cantkillserver
   #:err-restricted
   #:err-uniqoprivsneeded
   #:err-tslesschan
   #:err-nooperhost
   #:err-noservicehost
   #:err-nofeature
   #:err-badfeature
   #:err-badlogtype
   #:err-badlogsys
   #:err-badlogvalue
   #:err-isoperlchan
   #:err-chanownprivneeded
   #:err-umodeunknownflag
   #:err-usersdontmatch
   #:err-usernotonserv
   #:err-silelistfull
   #:err-toomanywatch
   #:err-badping
   #:err-badexpire
   #:err-dontcheat
   #:err-disabled
   #:err-listsyntax
   #:err-whosyntax
   #:err-wholimexceed
   #:err-remotepfx
   #:err-pfxunroutable
   #:err-badhostmask
   #:err-hostunavail
   #:err-usingsline
   #:err-statssline
   #:rpl-logon
   #:rpl-logoff
   #:rpl-watchoff
   #:rpl-watchstat
   #:rpl-nowon
   #:rpl-nowoff
   #:rpl-watchlist
   #:rpl-endofwatchlist
   #:rpl-watchclear
   #:rpl-islocop
   #:rpl-isnotoper
   #:rpl-endofisoper
   #:rpl-whoishost
   #:rpl-dcclist
   #:rpl-rules
   #:rpl-endofrules
   #:rpl-mapmore
   #:rpl-omotdstart
   #:rpl-omotd
   #:rpl-endofo
   #:rpl-settings
   #:rpl-endofsettings
   #:rpl-dumping
   #:rpl-dumprpl
   #:rpl-eodump
   #:rpl-traceroute-hop
   #:rpl-traceroute-start
   #:rpl-modechangewarn
   #:rpl-chanredir
   #:rpl-servmodeis
   #:rpl-otherumodeis
   #:rpl-endof-generic
   #:rpl-whowasdetails
   #:rpl-whoissecure
   #:rpl-unknownmodes
   #:rpl-cannotsetmodes
   #:rpl-luserstaff
   #:rpl-timeonserveris
   #:rpl-networks
   #:rpl-yourlanguageis
   #:rpl-language
   #:rpl-whoisstaff
   #:rpl-whoislanguage
   #:rpl-modlist
   #:rpl-endofmodlist
   #:rpl-helpstart
   #:rpl-helptxt
   #:rpl-endofhelp
   #:rpl-etracefull
   #:rpl-etrace
   #:rpl-knock
   #:rpl-knockdlvr
   #:err-toomanyknock
   #:err-chanopen
   #:err-knockonchan
   #:err-knockdisabled
   #:rpl-targumodeg
   #:rpl-targnotify
   #:rpl-umodegmsg
   #:rpl-omotdstart
   #:rpl-omotd
   #:rpl-endofomotd
   #:err-noprivs
   #:rpl-testmark
   #:rpl-testline
   #:rpl-notestline
   #:rpl-xinfo
   #:rpl-xinfostart
   #:rpl-xinfoend
   #:err-cannotdocommand
   #:err-cannotchangeumode
   #:err-cannotchangechanmode
   #:err-cannotchangeservermode
   #:err-cannotsendtonick
   #:err-unknownservermode
   #:err-servermodelock
   #:err-badcharencoding
   #:err-toomanylanguages
   #:err-nolanguage
   #:err-texttooshort
   #:err-numeric-err))

(defpackage #:maiden-irc
  (:nicknames #:org.shirakumo.maiden.clients.irc)
  (:use #:cl #:maiden #:maiden-networking #:maiden-client-entities)
  ;; client.lisp
  (:export
   #:irc-client
   #:nickname
   #:username
   #:password
   #:realname
   #:intended-nickname
   #:services)
  ;; commands.lisp
  (:export
   #:send-event
   #:message
   #:define-irc-command)
  ;; conditions.lisp
  (:export
   #:message-too-long-warning)
  ;; events.lisp
  (:export
   #:irc-event
   #:reply-event
   #:user
   #:user-user
   #:user-host
   #:unknown-event
   #:define-irc-reply))
