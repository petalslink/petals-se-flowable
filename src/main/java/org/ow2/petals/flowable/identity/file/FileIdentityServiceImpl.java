/**
 * Copyright (c) 2015-2020 Linagora
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
package org.ow2.petals.flowable.identity.file;

import java.util.List;
import java.util.Map;

import org.flowable.engine.common.api.FlowableException;
import org.flowable.idm.api.Group;
import org.flowable.idm.api.GroupQuery;
import org.flowable.idm.api.User;
import org.flowable.idm.api.UserQuery;
import org.flowable.idm.engine.impl.IdmIdentityServiceImpl;

public class FileIdentityServiceImpl extends IdmIdentityServiceImpl {

    /**
     * Users list into a map: key=user-id, value=user-password
     */
    private final Map<String, User> users;

    /**
     * Groups list into a map: key=group-id, value=list of user-id
     */
    private final Map<String, List<String>> groups;

    /**
     * Groups by user into a map: key=user-id, value=list of group-id
     */
    private final Map<String, List<Group>> groupsByUser;

    public FileIdentityServiceImpl(final Map<String, User> users, final Map<String, List<String>> groups,
            final Map<String, List<Group>> groupsByUser) {
        this.users = users;
        this.groups = groups;
        this.groupsByUser = groupsByUser;
    }

    @Override
    public UserQuery createUserQuery() {
        return new FileUserQueryImpl(this.users, this.groups);
    }

    @Override
    public GroupQuery createGroupQuery() {
        return new FileGroupQueryImpl(this.groupsByUser);
    }

    @Override
    public boolean checkPassword(final String userId, final String password) {
        final User user = this.users.get(userId);
        return Boolean.valueOf(user != null && password != null && password.equals(user.getPassword()));
    }

    @Override
    public User newUser(final String userId) {
        throw new FlowableException(
                "The user manager of the identity service based on file doesn't support creating a new user");
    }

    @Override
    public void saveUser(final User updatedUser) {
        throw new FlowableException(
                "The user manager of the identity service based on file doesn't support saving a user");
    }

    @Override
    public void deleteUser(final String userId) {
        throw new FlowableException(
                "The user manager of the identity service based on file doesn't support deleting a user");
    }

    @Override
    public Group newGroup(final String groupId) {
        throw new FlowableException(
                "The group manager of the identity service based on file doesn't support creating a new group");
    }

    @Override
    public void saveGroup(final Group updatedGroup) {
        throw new FlowableException(
                "The group manager of the identity service based on file doesn't support saving a group");
    }

    @Override
    public void deleteGroup(final String groupId) {
        throw new FlowableException(
                "The group manager of the identity service based on file doesn't support deleting a group");
    }
}
