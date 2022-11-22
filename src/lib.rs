use once_cell::sync::OnceCell;
shadow_rs::shadow!(build);

pub static APP_NAME: OnceCell<&str> = OnceCell::new();

pub static EMACS: OnceCell<bool> = OnceCell::new();

pub fn is_emacs() -> bool {
    *EMACS.get().unwrap()
}
