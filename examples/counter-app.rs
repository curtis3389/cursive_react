use cursive_react::{
    button, component, dialog, fullscreen_layer, props_component, stack, Callback, Element, React,
};
use std::sync::Arc;

fn main() {
    let react = React::new();
    react.render(component(counter_app));
}

fn counter_app(react: &mut React) -> Option<Element> {
    let (count, set_count) = react.use_state(0);
    Some(stack(vec![fullscreen_layer(dialog(
        "Test Title",
        vec![props_component(
            counter,
            CounterProps {
                count,
                on_submit: Arc::new(move |_| set_count(count + 1)),
            },
        )],
    ))]))
}

struct CounterProps {
    pub count: i32,
    pub on_submit: Arc<Callback>,
}

fn counter(_react: &mut React, props: &CounterProps) -> Option<Element> {
    let CounterProps { count, on_submit } = props;
    Some(button(format!("{}", count), on_submit.clone()))
}
