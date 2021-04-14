let term = Notty_unix.Term.create ()

let reducer (state, _msgs) =
      let view =
            match state with
            | State.View state -> View.draw state
            | State.Edit state -> Edit.draw state
            in

      Notty_unix.Term.image term view;

      let event = Notty_unix.Term.event term in
      let msgs =
            match state with
            | View state -> View.get_messages event state
            | Edit state -> Edit.get_messages event state
            in
      (state, msgs)

