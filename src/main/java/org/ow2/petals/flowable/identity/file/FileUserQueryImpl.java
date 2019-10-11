/**
 * Copyright (c) 2017-2019 Linagora
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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.flowable.common.engine.impl.interceptor.CommandContext;
import org.flowable.idm.api.User;
import org.flowable.idm.engine.impl.UserQueryImpl;

public class FileUserQueryImpl extends UserQueryImpl {

    private static final long serialVersionUID = -5324442830437611007L;

    /**
     * Users list into a map: key=user-id, value=user-password
     */
    private final transient Map<String, User> users;

    /**
     * Groups list into a map: key=group-id, value=list of user-id
     */
    private final transient Map<String, List<String>> groups;

    public FileUserQueryImpl(final Map<String, User> users, final Map<String, List<String>> groups) {
        this.users = users;
        this.groups = groups;
    }

    @Override
    public long executeCount(final CommandContext commandContext) {
        return executeQuery().size();
    }

    @Override
    public List<User> executeList(final CommandContext commandContext) {
        return executeQuery();
    }

    protected List<User> executeQuery() {
        final List<User> result = new ArrayList<>();
        if (this.getId() != null) {
            final User user = this.users.get(this.getId());
            if (user != null) {
                result.add(user);
            }
        } else if (this.getGroupId() != null) {
            final List<String> userIds = this.groups.get(this.getGroupId());
            if (userIds != null) {
                for (final String userId : userIds) {
                    result.add(this.users.get(userId));
                }
            }
        } else {
            result.addAll(this.users.values());
        }

        return result;
    }
}
