/**
 * Copyright (c) 2015 Linagora
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
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.identity.file;

import java.util.List;
import java.util.Map;

import org.activiti.engine.identity.Group;
import org.activiti.engine.identity.User;
import org.activiti.engine.impl.interceptor.Session;
import org.activiti.engine.impl.interceptor.SessionFactory;
import org.activiti.engine.impl.persistence.entity.UserIdentityManager;

public class FileUserEntityManagerFactory implements SessionFactory {

    /**
     * Users
     */
    final Map<String, User> users;

    /**
     * <p>
     * Groups by user.
     * </p>
     * <p>
     * The key is the user identifier, the value is the list of group identifier associated to the user/
     * </p>
     */
    final Map<String, List<Group>> groupsByUser;

    public FileUserEntityManagerFactory(final Map<String, User> users, final Map<String, List<Group>> groupsByUser) {
        this.users = users;
        this.groupsByUser = groupsByUser;
    }

    @Override
    public Class<?> getSessionType() {
        return UserIdentityManager.class;
    }

    @Override
    public Session openSession() {
        return new FileUserManager(this.users, this.groupsByUser);
    }

}
