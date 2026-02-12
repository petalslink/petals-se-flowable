/**
 * Copyright (c) 2015-2026 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.cloud.vacation.web;

import java.io.Serializable;

import org.ow2.petals.cloud.vacation.web.mvc.AbstractController;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.stereotype.Component;

/**
 * 
 * @author vnoel
 *
 */
@Component
@Scope(value = "session", proxyMode = ScopedProxyMode.TARGET_CLASS)
public class UserSession implements Serializable {

    private static final long serialVersionUID = -788073889518842353L;

    private String username;

    public String getUsername() {
        return username;
    }

    public void setUsername(final String username) {
        this.username = username;
    }

    /**
     * If {@link NeedUserException} is thrown, {@link AbstractController} will redirect to login page
     */
    public String getUsernameWithChecks() {
        if (this.username == null) {
            throw new NeedUserException();
        } else {
            return username;
        }
    }

    public static class NeedUserException extends RuntimeException {

        private static final long serialVersionUID = 5206911066170509965L;

    }
}
