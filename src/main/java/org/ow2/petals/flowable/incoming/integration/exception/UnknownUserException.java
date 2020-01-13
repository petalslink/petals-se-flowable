/**
 * Copyright (c) 2017-2020 Linagora
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
package org.ow2.petals.flowable.incoming.integration.exception;

import org.ow2.petals.components.flowable.generic._1.UnknownUser;

/**
 * No user found for the given identifier.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class UnknownUserException extends FaultException {

    private static final long serialVersionUID = -3518752050091536924L;

    private static final String MESSAGE_PATTERN = "No user found for the user identifier '%s'.";

    private final String userId;

    public UnknownUserException(final String userId) {
        super(String.format(MESSAGE_PATTERN, userId));
        this.userId = userId;
    }

    @Override
    public Object getBean() {
        final UnknownUser bean = new UnknownUser();
        bean.setId(this.userId);
        return bean;
    }

}
