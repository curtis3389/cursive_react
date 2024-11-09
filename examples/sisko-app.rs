use cursive::Cursive;
use cursive_react::{
    button, component, dialog, fullscreen_layer, hideable, horizontal_layout, layer, leaf, menubar,
    panel, props_component, stack, table, tree, vertical_layout, Callback, Column, Element, React,
    Row,
};
use std::collections::HashMap;
use std::sync::Arc;

fn main() {
    let react = React::new();
    react.render(component(sisko_app));
}

fn sisko_app(react: &mut React) -> Option<Element> {
    let (show_modal, set_show_modal) = react.use_state(false);
    let set_show_modal_copy = set_show_modal.clone();
    let mut layers: Vec<Element> = vec![fullscreen_layer(vertical_layout(vec![
        menubar(vec![
            tree(
                "File",
                vec![
                    leaf("Add Folder", Arc::new(|_| {})),
                    leaf("Add Files", Arc::new(|_| {})),
                    leaf(
                        "Exit",
                        Arc::new(|s| {
                            s.quit();
                        }),
                    ),
                ],
            ),
            tree("Edit", vec![leaf("todo", Arc::new(|_| {}))]),
            tree("View", vec![leaf("todo", Arc::new(|_| {}))]),
            tree("Options", vec![leaf("todo", Arc::new(|_| {}))]),
            tree("Tools", vec![leaf("todo", Arc::new(|_| {}))]),
            tree("Help", vec![leaf("About", Arc::new(|_| {}))]),
        ]),
        horizontal_layout(vec![component(left_panel), component(right_panel)]),
        component(bottom_panel),
        props_component(
            bottom_buttons,
            ButtonProps {
                on_submit: Arc::new(move |_| set_show_modal(true)),
            },
        ),
    ]))];
    if show_modal {
        layers.push(layer(dialog(
            "Test Modal",
            vec![button(
                "Okay".to_string(),
                Arc::new(move |_| set_show_modal_copy(false)),
            )],
        )))
    }
    Some(stack(layers))
}

fn left_panel(_: &mut React) -> Option<Element> {
    Some(hideable(panel(table(
        vec![],
        vec![],
        Arc::new(Cursive::noop),
        Arc::new(Cursive::noop),
    ))))
}

fn right_panel(_: &mut React) -> Option<Element> {
    Some(hideable(panel(table(
        vec![],
        vec![],
        Arc::new(Cursive::noop),
        Arc::new(Cursive::noop),
    ))))
}

fn bottom_panel(_: &mut React) -> Option<Element> {
    Some(hideable(panel(table(
        vec![
            Column::new("Tag"),
            Column::new("Original Value"),
            Column::new("New Value"),
        ],
        vec![Row::new(
            "0",
            HashMap::from([
                ("Tag", String::from("Test")),
                ("Original Value", String::from("orig")),
                ("New Value", String::from("new")),
            ]),
        )],
        Arc::new(Cursive::noop),
        Arc::new(Cursive::noop),
    ))))
}

struct ButtonProps {
    on_submit: Arc<Callback>,
}

fn bottom_buttons(_: &mut React, props: &ButtonProps) -> Option<Element> {
    let ButtonProps { on_submit } = props;
    Some(horizontal_layout(vec![
        button(String::from("Add Folder"), on_submit.clone()),
        button(String::from("Add Files"), Arc::new(Cursive::noop)),
        button(String::from("Cluster"), Arc::new(Cursive::noop)),
        button(String::from("Lookup"), Arc::new(Cursive::noop)),
        button(String::from("Scan"), Arc::new(Cursive::noop)),
        button(String::from("Save"), Arc::new(Cursive::noop)),
        button(String::from("Info"), Arc::new(Cursive::noop)),
        button(String::from("Remove"), Arc::new(Cursive::noop)),
        button(String::from("Lookup CD"), Arc::new(Cursive::noop)),
    ]))
}
