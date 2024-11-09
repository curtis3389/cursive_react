use cursive_react::{button, component, dialog, fullscreen_layer, stack, Element, React};
use std::sync::Arc;

fn main() {
    let react = React::new();
    react.render(component(stack_app));
}

fn stack_app(react: &mut React) -> Option<Element> {
    let (count, set_count) = react.use_state(0);
    let set_count_b = set_count.clone();
    let mut dialog_a = fullscreen_layer(dialog(
        "Dialog A",
        vec![button(
            String::from("Swap"),
            Arc::new(move |_| set_count(1)),
        )],
    ));
    let mut dialog_b = fullscreen_layer(dialog(
        "Dialog B",
        vec![button(
            String::from("Swap"),
            Arc::new(move |_| set_count_b(0)),
        )],
    ));
    dialog_a.props.key = Some("dialog_a".to_string());
    dialog_b.props.key = Some("dialog_b".to_string());
    let layers = if count == 0 {
        vec![dialog_b, dialog_a]
    } else {
        vec![dialog_a, dialog_b]
    };
    Some(stack(layers))
}
