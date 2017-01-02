/**
 * Copyright (c) 2015-2017 Linagora
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
package org.ow2.petals.activitibpmn.identity.file;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.activiti.engine.identity.Group;
import org.activiti.engine.identity.User;

public class UserGroupIdCriteria implements UserCriteria {

    /**
     * Groups by user into a map: key=user-id, value=list of group-id
     */
    private final Map<String, List<Group>> groupsByUser;

    /**
     * Group identifier on which users must be filtered
     */
    private final String expectedGroupId;

    public UserGroupIdCriteria(final Map<String, List<Group>> groupsByUser, final String expectedGroupId) {

        assert groupsByUser != null;
        assert expectedGroupId != null;

        this.groupsByUser = groupsByUser;
        this.expectedGroupId = expectedGroupId;
    }

    @Override
    public List<User> meetCriteria(final List<User> users) {

        final List<User> usersOfGroup = new ArrayList<User>();

        for (final User user : users) {
            final List<Group> groupsOfUser = this.groupsByUser.get(user.getId());
            for (final Group groupOfUser : groupsOfUser) {
                if (groupOfUser != null && this.expectedGroupId.equals(groupOfUser.getId())) {
                    usersOfGroup.add(user);
                    break;
                }
            }
        }
        return usersOfGroup;
    }

}
